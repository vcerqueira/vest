#' arima baseline
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
      x <- ts(c(train,test[seq_len(i-1)]), frequency = freq)

      refit <- Arima(x, model=fit)

      fcmat[i,] <- as.vector(forecast::forecast(refit, h=h)$mean)
    }

    fcmat
  }

