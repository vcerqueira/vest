#' Xgb predictions
#'
#' @param model xgb model
#' @param newdata new data
#'
#' @import xgboost
#'
#' @export
get_xgb_preds <-
  function(model, newdata) {
    Y <- get_y_val(model$form,newdata)
    valid_cols <- which(colnames(newdata) %in% model$cn)

    newdata  <- subset(newdata, select = valid_cols)
    newdata  <- stats::model.matrix(model$form, newdata)

    dtest <- xgboost::xgb.DMatrix(newdata, label = Y)

    predict(model, dtest)
  }

#' Create xgb model using a grid search
#'
#' @param form formula
#' @param data training data, data.frame
#'
#' @import xgboost
#'
#' @export
get_xgb_model <-
  function(form, data) {
    GS <-
      expand.grid(
        eta = c(.1,.25,.5),
        max_depth = c(3,5,7,10),
        #nrounds = c(10,25),
        booster=c("gblinear","gbtree")
      )

    cn <- colnames(data)
    y <- get_y_val(form, data)
    X <- stats::model.matrix(form, data)

    Xy <- xgboost::xgb.DMatrix(X, label = y)

    params <- xgb.optimize(X, y, gsearch = GS)
    sresult <- params$search_results
    params <- params$best_parameters

    fparams <-
      list(
        max_depth = params$max_depth,
        eta = params$eta,
        silent = 1,
        objective = "reg:linear",
        eval_metric = "rmse"
      )

    model <-
      xgb.train(
        params = fparams,
        booster = as.character(params$booster),
        data = Xy,
        nrounds = params$nrounds,
        verbose = 0
      )

    model$cn <- cn
    model$form <- form

    model
  }


#' Optimizing xgb
#'
#' @param X predictors
#' @param y target
#' @param gsearch grid search
#'
#' @import xgboost
#'
#' @export
xgb.optimize <-
  function(X, y, gsearch) {
    l <- nrow(X)
    train_ids <- 1:ceiling(l * .7)

    X_tr <- X[train_ids,]
    X_ts <- X[-train_ids,]
    y_tr <- y[train_ids]
    y_ts <- y[-train_ids]

    tr <- xgboost::xgb.DMatrix(as.matrix(X_tr), label = y_tr)
    tst <- xgboost::xgb.DMatrix(as.matrix(X_ts), label = y_ts)

    wlist <- list(eval = tst, train = tr)

    L <- numeric(nrow(gsearch))
    NROUNDS <- numeric(nrow(gsearch))
    for (i in 1:nrow(gsearch)) {
      eta_i <- gsearch[i, "eta"]
      md_i <- gsearch[i, "max_depth"]
      algo_i <- as.character(gsearch[i, "booster"])

      params_i  <-
        list(
          max_depth = md_i,
          eta = eta_i,
          silent = 1,
          objective = "reg:linear",
          eval_metric = "rmse"
        )

      model <-
        xgboost::xgb.train(
          params = params_i,
          booster = algo_i,
          data = tr,
          nrounds = 50,
          watchlist = wlist,
          verbose = 0
        )

      eval.log <- model$evaluation_log

      val.loss <- eval.log[[2]]
      L[i] <- min(val.loss)
      NROUNDS[i] <- which.min(val.loss)
    }

    best_var <- which.min(L)
    nrounds_best <- NROUNDS[best_var]

    eta <- gsearch[best_var, "eta"]
    max_depth <- gsearch[best_var, "max_depth"]
    booster <- as.character(gsearch[best_var, "booster"])

    best_parameters <-
      list(
        eta = eta,
        max_depth = max_depth,
        nrounds = nrounds_best,
        booster = booster
      )

    search_results <-
      list(L=L,
           NROUNDS=NROUNDS)

    list(best_parameters=best_parameters,
         search_results=search_results)
  }
