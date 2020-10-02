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
  function(form, train, iterations=25) {
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


#' wrappersadda
#'
#' @param form form
#' @param train sadasda
#' @param test sasa
#' @param ... sadada
#'
#' @export
M5.cycle <-
  function(form, train, test, ...) {
    model <- M5.train(form, train, ...)

    M5.predict(model, test)
  }

#' wrapperq
#'
#' @param form forms
#' @param train sadasdsa
#' @param ... sadadas
#'
#' @import Metrics
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


#' lasso
#'
#' @param form formula
#' @param train train
#'
#' @import glmnet
#'
#' @export
LASSO.train <-
  function(form, train) {
    #SC <- soft_completion(train)
    if (any(is.na(train))) {
      stop("misssing values in train data")
    }

    alpha <- 1

    x <- model.matrix(form, train)
    y <- get_y_val(form, train)

    m0 <- cv.glmnet(x,y, alpha=alpha,family = "gaussian")

    m <-
      glmnet(x,
             y,
             alpha = alpha,
             family = "gaussian",
             lambda = m0$lambda.min)

    m$form <- form

    m
  }


#' glme pred
#'
#' @param model model
#' @param test test
#'
#' @import glmnet
#'
#' @export
LASSO.predict <-
  function(model, test) {
    if (any(is.na(test))) {
      stop("misssing values in test data")
    }

    #x <- model.matrix(model$form, test)
    cols_tst <- colnames(test)
    cols_in_m <- model$beta@Dimnames[[1]]

    model_names <- cols_tst[cols_tst %in% cols_in_m]
    model_names <- c(model_names,as.character(model$form[[2]]))

    testx <- test[,model_names]

    x <- model.matrix(model$form, testx)

    unname(predict(model, x)[,1])
  }
