#' XGB training function
#'
#' @param form formula
#' @param train train set, data.frame
#'
#' @export
XGB.train <-
  function(form, train) {
    if (any(is.na(train))) {
      stop("Misssing values in train data")
    }

    model <- get_xgb_model(form,train)

    model
  }


#' XGB predict function
#'
#' @param model model from XGB.train fun
#' @param test test set, data.frame
#'
#' @export
XGB.predict <-
  function(model, test) {
    if (any(is.na(test))) {
      stop("Misssing values in test data")
    }

    preds <- get_xgb_preds(model, test)

    preds
  }

#' Model tree using cubist
#'
#' @param form formula
#' @param train train set, data.frame
#' @param iterations no. boosting iterations
#'
#' @import Cubist
#'
#' @export
M5.train <-
  function(form, train, iterations=5) {
    if (any(is.na(train))) {
      stop("Misssing values in train data")
    }

    x <- stats::model.matrix(form, train)
    y <- get_y_val(form, train)

    m <- suppressWarnings(cubist(x, y, committees = iterations))
    m$form <- form

    m
  }


#' Model tree prediction function
#'
#' @param model model from M5.train
#' @param test test, data.frame
#'
#' @import Cubist
#'
#' @export
M5.predict <-
  function(model, test) {
    if (any(is.na(test))) {
      stop("Misssing values in test data")
    }

    x <- stats::model.matrix(model$form, test)

    predict(model, x)
  }


#' Wrapper for M5 model
#'
#' @param form formula
#' @param train train set, data.frame
#' @param test test set, data.frame
#' @param ... other parameters to m5.train
#'
#' @export
M5.cycle <-
  function(form, train, test, ...) {
    model <- M5.train(form, train, ...)

    M5.predict(model, test)
  }

#' Wrapper for M5 cycle using training data
#'
#' @param form formula
#' @param train train set, data.frame
#' @param ... other pars, to M5.cycle
#'
#' @export
M5.self_cycle <-
  function(form, train,...) {
    splits <- ts_holdout(1:nrow(train), .8)

    trainx <- train[splits$train, ]
    testx <- train[splits$test, ]

    yh <- M5.cycle(form, trainx, testx, ...)
    y <- get_y_val(form, testx)

    Metrics::rmse(y, yh)
  }


#' Wrapper M5 multistep cycle
#'
#' @param train train set, data.frame
#' @param test test set, data.frame
#' @param h forecasting horizon
#'
#' @export
M5.multi_step_cycle <-
  function(train, test, h) {
    form <- target~ .

    targets <- paste0("target",1:h)
    tgti <- which(colnames(train) %in% targets)

    X <- train[,-tgti]
    y <- train[,tgti]

    X.test <- test[,-tgti]
    X.test.tgt <- cbind.data.frame(X.test,target=-1)

    train1 <- cbind.data.frame(X, target=y[,"target1"])

    model <- M5.train(form, train1, iterations = 5)

    x <- X.test.tgt
    yhat <- matrix(NA, nrow=nrow(x), ncol=h)
    yhat[,1] <- M5.predict(model, x)
    for (i in 2:h) {
      x[,1:(ncol(x)-1)] <- x[,2:ncol(x)]
      x[,"tm1"] <- yhat[,i-1]

      yhati <- M5.predict(model, x)

      yhat[,i] <- yhati
    }
    colnames(yhat) <- colnames(y)

    yhat
  }

