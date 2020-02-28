#' Multi step prediction with updated dynamics
#'
#' @param model Predictive model
#' @param x Past lags in the test data
#' @param dyns Dynamics in the test data (for h=1)
#' @param h Forecasting horizon
#' @param update.dynamics logical. If true, dynamics are updated for h>1
#' @param ft_model Feat engineer model for updating dynamics
#' @param model_cols Names of the features in trained model
#' @param predfun Prediction fun for making predictions using model
#'
#' @export
multistep.prediction <-
  function(model,
           x,
           dyns,
           h,
           ft_model,
           update.dynamics=TRUE,
           model_cols,
           predfun) {

    # Prepare data
    tgt_cols <- paste0("target", 1:h)
    tgt_main <- "target1"
    ms_cols <- setdiff(tgt_cols, tgt_main)
    ms_col_ids <- !colnames(x) %in% tgt_cols

    X <- xnext <- x[,ms_col_ids]

    xnext <- cbind.data.frame(xnext, dyns)
    xnext <- cbind.data.frame(xnext, -1)
    colnames(xnext)[ncol(xnext)] <- tgt_main

    # Initialize matrix of predictions
    yhat <- matrix(NA, nrow=nrow(x), ncol=h)
    colnames(yhat) <- tgt_cols
    
    # Predict h=1
    yhat[,1] <- predfun(model, xnext)

    keys <- ft_model@keys
    fourier_vals <- vector("list", nrow(xnext))
    for (i in 1:nrow(X)) {
      y.known <- X[1:i,"tm1"]
      y.all <- c(keys$y_tr_tm1, y.known)

      fourier_vals[[i]] <-
        get_fourier_terms_(y = y.all,
                           nterms = keys$n_f_terms,
                           freq = keys$freq,
                           h = h)
    }

    dhr.cols <- grep("DHR.", colnames(xnext))
    if (length(dhr.cols) > 0) {
      dhr.names <- colnames(xnext)[dhr.cols]
    }

    # h>2
    if (update.dynamics) {
      # When we update dyns with predicted values
      for (j in 2:h) {
        cat("Forecasting horizon:", j, "\n")

        X[, 1:(ncol(X) - 1)] <- X[, 2:ncol(X)]
        X[, "tm1"] <- yhat[, j - 1]

        if (any(is.infinite(X[,"tm1"]))) {
          X[,"tm1"][is.infinite(X[,"tm1"])] <- NA_real_
        }

        if (any(is.na(X[,"tm1"]))) {
          X[,"tm1"][is.na(X[,"tm1"])] <- X[,"tm2"][is.na(X[,"tm1"])]
        }

        DYNSi <- predict(ft_model, X)
        

        Xi <- cbind.data.frame(X, DYNSi)
        Xi <- cbind.data.frame(Xi,-1)
        colnames(Xi)[ncol(Xi)] <- tgt_main

        if (!is.null(model_cols)) {
          Xi <- Xi[, colnames(Xi) %in% model_cols]
        }

        if (length(dhr.cols) > 0) {
          f.vals <-
            lapply(fourier_vals,
                   function(tp) {
                     tp[j - 1, ]
                   })

          f.vals <- as.data.frame(do.call(rbind, f.vals))
          f.vals.sub <- f.vals[,dhr.names,drop=FALSE]

          Xi[,dhr.names] <- f.vals.sub
        }

        yhat[, j] <- predfun(model, Xi)
      }
    } else {
      # When we do not update dyns with predicted values
      for (j in 2:h) {
        cat("Forecasting horizon:", j, "\n")
        X[, 1:(ncol(X) - 1)] <- X[, 2:ncol(X)]
        X[, "tm1"] <- yhat[, j - 1]

        Xi <- cbind.data.frame(X, dyns)
        Xi <- cbind.data.frame(Xi,-1)
        colnames(Xi)[ncol(Xi)] <- tgt_main

        if (!is.null(model_cols)) {
          Xi <- Xi[, colnames(Xi) %in% model_cols]
        }

        yhat[, j] <- predfun(model, Xi)
      }
    }

    yhat
  }
