#' WF part 1
#'
#' @param x time series, ts class
#'
#' @export
WF_part1 <-
  function(x) {
    h <- 24

    frq <- stats::frequency(x)

    xs <- ts_holdout(x, .8, frq = frq)

    cat("Estimating best k\n")
    klag <- search_k_vall(xs$train, max_k = 30)
    if (klag < 10) {
      klag <- 10
    }

    ds <- create_datasets(xs$train,xs$test,
                          klag+h, h=h)

    train0 <- ds$train
    test0 <- ds$test

    tgts <- paste0("target",1:h)

    cat("FE\n")
    train_feats <-
      feature_engineering(x = train0,
                          targets = tgts,
                          freq = frq)

    testX <- test0[,-grep("^target", colnames(test0))]
    cat("FE test\n")
    test_dynamics <- predict(train_feats, testX)

    stopifnot(all.equal(colnames(test_dynamics),
                        colnames(train_feats@Dynamics)))

    list(data=ds, feature_model=train_feats,
         test=testX, test_dynamics=test_dynamics)
  }

#' WF part2
#'
#' @param x results from part1
#' @param learning_functions list with 2 elements.
#' the frist element is the training func, and
#' the second element is the predict func
#'
#' @export
WF_part2 <-
  function(x, learning_functions) {
    training_fun <- learning_functions[[1]]
    predict_fun <- learning_functions[[2]]

    h <- 24

    train_feats <- x$feature_model
    test_dynamics <- x$test_dynamics
    testX <- x$test
    train0 <- x$data$train
    test0 <- x$data$test

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
    # All dynamics, except bad ones (NAs, NZV)
    ALL.FEATS <- cbind.data.frame(LAGS, D0)
    # all
    TR_ALL <- cbind.data.frame(LAGS, D0, y)

    cat("vest var select\n")
    FS1 <- feat_select_vest(train_feats)
    cat("vest br alt\n")
    FS2 <- feat_select_br(train_feats)

    D_SET <-
      list(
        FS1 = FS1,
        FS2 = FS2
      )

    TR_D <-
      lapply(D_SET,
             function(Z) {
               cbind.data.frame(LAGS, Z, y)
             })
    names(TR_D) <- paste0("TR_",names(TR_D))

    TR_SET <-
      list(TR_VANILLA=TR_VANILLA,
           TR_DYNS=TR_DYNS,
           TR_ALL=TR_ALL)

    TR_SET <- c(TR_SET, TR_D)

    cat("Learning cycles\n")
    YHATF <- vector("list",length(TR_SET))
    YHATT <- vector("list",length(TR_SET))
    for (i in 1:length(TR_SET)) {
      cat(names(TR_SET)[i],"\n")
      cat("Training\n")
      M <- training_fun(nextform, TR_SET[[i]])

      used_cols <- colnames(TR_SET[[i]])

      d.ids <- which(colnames(test_dynamics) %in% used_cols)

      TS_DYNS <- test_dynamics[,d.ids]

      cat("Testing\n")
      cat("...Static\n")
      yhati_f <-
        multistep.prediction(
          model = M,
          x = testX,
          dyns = TS_DYNS,
          h = h,
          ft_model = train_feats,
          update.dynamics = FALSE,
          model_cols = used_cols,
          predfun = predict_fun
        )

      cat("...Dynamic\n")
      yhati_t <-
        multistep.prediction(
          model = M,
          x = testX,
          dyns = TS_DYNS,
          h = h,
          ft_model = train_feats,
          update.dynamics = TRUE,
          model_cols = used_cols,
          predfun = predict_fun
        )

      YHATF[[i]] <- yhati_f
      YHATT[[i]] <- yhati_t
    }

    names(YHATF) <- paste0(names(TR_SET),"_ST")
    names(YHATT) <- paste0(names(TR_SET),"_DY")

    y_tr <- train0[,grep("^target", colnames(train0))]
    y_ts <- test0[,grep("^target", colnames(test0))]

    yhatl <- c(YHATF, YHATT)

    list(yhat=yhatl, y=y_ts, y_tr=y_tr,
         feats_model=train_feats)
  }




