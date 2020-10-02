#' WF part 1
#' Getting the VEST features
#'
#' @param train time series, ts class
#' @param test test time series, ts class
#' @param h forecasting horizon
#'
#' @export
WF_part1 <-
  function(train, test, h) {
    frq <- stats::frequency(train)

    k_lag <- search_k_vall(as.vector(train), max_k = 30)
    if (k_lag < 10) {
      k_lag <- 10
    }

    ds <- create_datasets(train,test,
                          k_lag+h, h=h)

    train0 <- ds$train
    test0 <- ds$test

    y_tr <- train0$target
    y_ts <- test0$target

    tgts <- paste0("target",1:h)

    cat("Feature Engineering\n")
    train_feats <-
      feature_engineering(x = train0,
                          targets = tgts,
                          freq = frq)

    testX <- test0[,-grep("^target", colnames(test0))]

    cat("Feature Engineering on test\n")
    test_dynamics <- predict(train_feats, testX)

    stopifnot(all.equal(colnames(test_dynamics),
                        colnames(train_feats@Dynamics)))

    list(data=ds, feature_model=train_feats,
         test=testX, test_dynamics=test_dynamics)
  }


#' Workflow part 2
#' Training forecasting models
#' MSF with a direct approach
#'
#' @param x results from wf pt1
#' @param h horizon
#' @param learning_function learning functions.
#' A list: first element is the training function, second element is the predict funcions
#'
#' @export
WF_part2_direct2 <-
  function(x, h, learning_functions) {
    training_fun <- learning_functions[[1]]
    predict_fun <- learning_functions[[2]]

    train_feats0 <- train_feats <- x$feature_model
    test_dynamics <- x$test_dynamics
    testX <- x$test
    train0 <- x$data$train
    test0 <- x$data$test

    y_tr <- train0[,grep("^target", colnames(train0))]
    y_ts <- test0[,grep("^target", colnames(test0))]

    nextform <- target1 ~.
    main_target <- as.character(nextform[[2]])
    cn <- colnames(train_feats@y)

    LAGS <- train_feats@X
    y <- train_feats@y[,main_target,drop=FALSE]
    D0 <- train_feats@Dynamics

    # Benchmark -- pure AR
    TR_VANILLA <- cbind.data.frame(LAGS, y)
    # Baseline -- only dynamics
    TR_DYNS <- cbind.data.frame(D0, y)

    cat("Feature Selection\n")
    FS1 <- feat_select_bf(x = train_feats)
    FS2 <- feat_select_corr(x = train_feats)
    FS3 <- feat_select_br(x = train_feats)

    D_SET <-
      list(
        BF=FS1,
        CORR = FS2,
        BR = FS3
      )

    TR_D <-
      lapply(D_SET,
             function(Z) {
               cbind.data.frame(LAGS, Z, y)
             })
    names(TR_D) <- paste0("TR_",names(TR_D))

    TR_SET <-
      list(TR_VANILLA=TR_VANILLA,
           TR_DYNS=TR_DYNS)

    TR_SET <- c(TR_SET, TR_D)

    cat("Learning cycles\n")
    YHATT <- vector("list",length(TR_SET))
    print(names(TR_SET))
    for (i in 1:length(TR_SET)) {
      cat(names(TR_SET)[i],"\n")
      cat("Training\n")
      train_feats_use <- train_feats0

      M <- training_fun(nextform, TR_SET[[i]])
      used_cols <- colnames(TR_SET[[i]])
      d.ids <- which(colnames(test_dynamics) %in% used_cols)
      TS_DYNS <- test_dynamics[,d.ids, drop=F]

      yhati_t <-
        direct_msf(
          X_tr = TR_SET[[i]],
          X_ts_lags = testX,
          X_ts_dynamics = test_dynamics,
          y_tr = y_tr,
          h = h,
          training_fun = training_fun,
          predict_fun = predict_fun
        )

      YHATT[[i]] <- yhati_t
    }

    y_tr <- train0[,grep("^target", colnames(train0))]
    y_ts <- test0[,grep("^target", colnames(test0))]

    names(YHATT) <- names(TR_SET)

    yhatl <- YHATT

    list(yhat=yhatl, y=y_ts, y_tr=y_tr,
         feats_model=train_feats)
  }


