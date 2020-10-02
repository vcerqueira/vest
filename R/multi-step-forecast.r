#' Direct multi-step forecasting
#'
#' @param X_tr assad
#' @param X_ts_lags sss
#' @param X_ts_dynamics as
#' @param y_tr as
#' @param h ss
#' @param training_fun sas
#' @param predict_fun ss
#'
#' @export
#'
#'
direct_msf <-
  function(X_tr, X_ts_lags, X_ts_dynamics, y_tr, h, training_fun, predict_fun) {
    tgt_idx_tr <- grep("target", colnames(X_tr))
    tgt_idx_ts <- grep("target", colnames(X_ts_lags))

    if (length(tgt_idx_tr) > 0) {
      X_tr <- X_tr[,-tgt_idx_tr]
    }

    if (length(tgt_idx_ts) > 0) {
      X_ts_lags <- X_ts_lags[,-tgt_idx_ts]
    }

    used_attrs <- colnames(X_tr)

    X_ts <- cbind.data.frame(X_ts_lags, X_ts_dynamics)
    attrs_idx <- which(colnames(X_ts) %in% used_attrs)
    X_ts <- X_ts[,attrs_idx, drop=F]


    yhat <- matrix(NA, nrow=nrow(X_ts), ncol=h)
    for (horizon_ in 1:h) {
      cat("Training with h:",horizon_,"\n")

      train_h <-
        cbind.data.frame(X_tr,
                         target = y_tr[,horizon_])

      test_h <-
        cbind.data.frame(X_ts, target = -1)


      M <- training_fun(target ~., train_h)

      yhat[, horizon_] <- predict_fun(M, test_h)
    }

    yhat
  }

