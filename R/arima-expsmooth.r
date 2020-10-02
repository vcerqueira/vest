#' ARIMA MODEL
#'
#' @param train train series, ts
#' @param test test series, ts
#' @param freq frequency
#' @param h horizon
#'
#' @import forecast
#'
#' @export
arima_multistep <-
  function(train,test,freq,h) {
    train <- stats::ts(train, frequency = freq)
    test <- stats::ts(test, frequency = freq)

    n <- length(test)
    fit <- auto.arima(train)

    fcmat <- matrix(0, nrow=n, ncol=h)
    for(i in 1:n) {
      cat(i,"/",n,"\n")
      x <- stats::ts(c(train,test[seq_len(i-1)]), frequency = freq)

      refit <- Arima(x, model=fit)

      pred <- as.vector(forecast::forecast(refit, h=h)$mean)

      fcmat[i,] <- pred
    }

    fcmat
  }



#' ETS MODEL
#'
#' @param train train series, ts
#' @param test test series, ts
#' @param freq frequency
#' @param h horizon
#'
#' @import forecast
#'
#' @export
ets_multistep <-
  function(train,test,freq,h) {
    train <- stats::ts(train, frequency = freq)
    test <- stats::ts(test, frequency = freq)

    n <- length(test)
    fit <- ets(train)

    fcmat <- matrix(0, nrow=n, ncol=h)
    for(i in 1:n) {
      cat(i,"/",n,"\n")
      x <- stats::ts(c(train,test[seq_len(i-1)]), frequency = freq)

      refit <- ets(x, model=fit)

      pred <- as.vector(forecast::forecast(refit, h=h)$mean)

      fcmat[i,] <- pred
    }

    fcmat
  }



#' TBATS MODEL
#'
#' @param train train series, ts
#' @param test test series, ts
#' @param freq frequency
#' @param h horizon
#'
#' @import forecast
#'
#' @export
tbats_multistep <-
  function(train,test,freq,h) {
    train <- stats::ts(train, frequency = freq)
    test <- stats::ts(test, frequency = freq)

    n <- length(test)
    fit <- tbats(train)

    fcmat <- matrix(0, nrow=n, ncol=h)
    for(i in 1:n) {
      cat(i,"/",n,"\n")
      x <- stats::ts(c(train,test[seq_len(i-1)]), frequency = freq)

      refit <- tbats(x, model=fit)

      pred <- as.vector(forecast::forecast(refit, h=h)$mean)

      fcmat[i,] <- pred
    }

    fcmat
  }


#' run clasical methds
#'
#' @param y_train num
#' @param y_test num
#' @param freq frq
#' @param h h
#'
#' @export
run_classical_methods <-
  function(y_train,y_test,freq, h) {

    cat("ARIMA")
    capture.output(yhat_arima <-
      arima_multistep(
        train = y_train,
        test = y_test,
        freq = freq,
        h = h
      ))

    cat("ETS")
    capture.output(yhat_ets <-
      ets_multistep(
        train = y_train,
        test = y_test,
        freq = freq,
        h = h
      ))

    cat("TBATS")
    capture.output(yhat_tbats <-
      tbats_multistep(
        train = y_train,
        test = y_test,
        freq = freq,
        h = h
      ))

    tgt_cols <- paste0("target", 1:h)
    colnames(yhat_arima) <-
      colnames(yhat_ets) <-
      colnames(yhat_tbats) <- tgt_cols

    list(ARIMA = yhat_arima,
         ETS = yhat_ets,
         TBATS = yhat_tbats)
  }

