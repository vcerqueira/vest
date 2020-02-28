#' Get importance generic
#'
#' @param object object of class VEST
#'
#' @export
setGeneric("get_importance",
           function(object) {
             standardGeneric("get_importance")
           })

#' Get importance scores
#'
#' @param object object of class VEST
#'
#' @export
setMethod("get_importance",
          signature("VEST"),
          function(object) {

            X <- object@X
            y <- object@y
            cn <- colnames(y)
            Dyns <- object@Dynamics

            FI <-
              lapply(cn,
                     function(tgt) {
                       y_tgt <- y[, tgt, drop = FALSE]
                       DF <- cbind.data.frame(X, Dyns, y_tgt)

                       form <- as.formula(paste0(tgt, " ~."))

                       estimate_fi(form, DF)
                     })

            names(FI) <- cn

            object@importance <- FI

            object
          })


#' Get summary operations generic
#'
#' @param object object of class VEST
#'
#' @export
setGeneric("get_summary",
           function(object) {
             standardGeneric("get_summary")
           })

#' Get summary operations
#'
#' @param object object of class VEST
#'
#' @export
setMethod("get_summary",
          signature("VEST"),
          function(object) {
            cat("Get Id Summary\n")
            capture.output(S_O <- apply(object@X, 1, ft_summary))
            S_O <- t(S_O)

            l <- length(object@Z)
            nms <- names(object@Z)
            nms <- gsub("T_","",nms)

            S_l <- vector("list", l)
            for (i in 1:l) {
              cat("Summary of",names(object@Z[i]),"\n")
              capture.output(
                S_i <-
                  apply(object@Z[[i]], 1, ft_summary)
              )

              S_i <- t(S_i)

              colnames(S_i) <- gsub("^O\\.","",colnames(S_i))
              colnames(S_i) <- paste0(nms[i],".",colnames(S_i))

              S_l[[i]] <- S_i
            }

            S_l <- do.call(cbind, S_l)

            DYNAMICS <- cbind(S_O, S_l)
            DYNAMICS <- as.data.frame(DYNAMICS)

            object@Dynamics <- DYNAMICS

            object
          })


#' Feature clean up generic
#'
#' @param object object of class VEST
#'
#' @export
setGeneric("cleanup_feats",
           function(object) {
             standardGeneric("cleanup_feats")
           })

#' Feature clean up method
#'
#' @param object object of class VEST
#'
#' @import caret
#' @import DMwR
#'
#' @export
setMethod("cleanup_feats",
          signature("VEST"),
          function(object) {
            bad_feats <- numeric()

            if (any(sapply(object@Dynamics, is.infinite))) {
              object@Dynamics <- replace_inf(object@Dynamics)
            }

            rm_NA <- manyNAs(t(object@Dynamics), .7)

            bad_feats <- c(bad_feats,unname(rm_NA))

            rm_nzv <-
              nearZeroVar(object@Dynamics,uniqueCut = .5)

            bad_feats <- sort(unique(c(bad_feats,rm_nzv)))

            if (length(bad_feats) > 0) {
              object@Dynamics <- object@Dynamics[,-bad_feats]
            }

            object@keys[["bad_feats"]] <- bad_feats

            if (any(is.na(object@Dynamics))) {
              object@Dynamics <- soft_completion(object@Dynamics)$x
            }

            typical_vals <- apply(object@Dynamics, 2, median)

            object@keys[["typical_vals"]] <- typical_vals

            object
          })


#' Features summary
#'
#' @param x num vec
#'
#' @export
ft_summary <-
  function(x) {
    x[is.infinite(x)] <- NA_real_
    x[is.na(x)] <- median(x, na.rm=TRUE)

    if (all(is.na(x))) {
      return(
        c(
          O.RD = NA,
          O.VAR = NA,
          O.SUM = NA,
          O.SK = NA,
          O.KRT = NA,
          O.LP = NA,
          O.ACC1 = NA,
          O.ACC2 = NA,
          O.PCARE1 = NA,
          O.PCARE2 = NA,
          O.MEAN = NA,
          O.MDN = NA,
          O.MIN = NA,
          O.MAX = NA,
          O.SDEV = NA,
          O.MLE = NA,
          O.HURST1 = NA,
          O.HURST2 = NA,
          O.BP = NA,
          O.P05 = NA,
          O.P95 = NA,
          O.IQR = NA,
          O.SLP = NA,
          O.NORM = NA,
          O.PACF = NA,
          O.ACF = NA,
          O.NO = NA,
          O.AMP = NA,
          O.TP = NA,
          O.STEP = NA,
          O.PEAKD = NA,
          O.PEAKI = NA
        )
      )
    }

    cat("Computing rel dispersion ...\n")
    D.relative_dispersion <- relative_dispersion(x)

    cat("Computing skewness ...\n")
    D.skewness <- moments::skewness(x)

    cat("Computing kurtosis ...\n")
    D.kurtosis <- moments::kurtosis(x)

    cat("Computing acceleration .. \n")
    D.accl <- accelaration(x)

    cat("Computing mean ...\n")
    D.mean <- mean(x)

    cat("Computing median ...\n")
    D.median <- stats::median(x)

    cat("Computing min ...\n")
    D.min <- min(x)

    cat("Computing max ...\n")
    D.max <- max(x)

    cat("Computing standard deviation ...\n")
    D.sdev <- stats::sd(x)

    cat("Computing var ...\n")
    D.var <- stats::var(x)

    cat("Computing sum ...\n")
    D.sum <- sum(x)

    cat("Computing rel var ...\n")
    D.relvar <- as.integer(D.var > D.sdev)


    cat("Computing maximum lyapunov exponent ...\n")
    D.mle <-
      tryCatch(max_lyapunov_exp(x),
               error = function(e) NA)

    cat("Computing hurst ...\n")
    D.hurst1 <-
      tryCatch(
        HURST(x, nmoments = 1),
        error = function(e) {
          NA
        }
      )

    D.hurst2 <-
      tryCatch(
        HURST(x, nmoments = 2),
        error = function(e) {
          NA
        }
      )

    cat("Computing serial correlation ...\n")
    D.box <-
      tryCatch(
        stats::Box.test(x)$p.val,
        error = function(e)
          NA
      )

    cat("Computing qtl ...\n")
    D.qt05 <- unname(stats::quantile(x, .05))

    cat("Computing qtl 2 ...\n")
    D.qt95 <- unname(stats::quantile(x, .95))

    cat("Computing IQR ...\n")
    D.iqr <- stats::IQR(x)

    cat("Computing Slope ...\n")
    D.slp <- slope(x)

    cat("Computing Norm ...\n")
    D.norm <- norm(t(x))

    cat("Computing (P)ACF\n")
    D.pacf <- stats::pacf(x, plot=FALSE, lag.max = 1)$acf[,,1]
    D.acf <- mean(stats::acf(x, plot=F)$acf[-1,,1])

    cat("Computing Number of outliers 2\n")
    D.nout2 <- nout2(x)

    cat("Computing FFT Strength\n")
    D.strg <- fft_strength(x)

    cat("Computing turning points\n")
    D.tp <- tpoints(x)

    cat("Computing step change\n")
    D.step <- step_change(x)

    cat("Computing number of peaks\n")
    D.npeak <- npeaks(x)

    cat("Computing poincare variability\n")
    D.poinc <- unname(poincare_variability(x))

    cat("Getting lp\n")
    D.lp <- unname(x[length(x)])

    DYNAMICS <-
      c(
        O.RD = D.relative_dispersion,
        O.VAR = D.var,
        O.SUM = D.sum,
        O.SK = D.skewness,
        O.KRT = D.kurtosis,
        O.LP = D.lp,
        O.ACC1 = unname(D.accl[1]),
        O.ACC2 = unname(D.accl[2]),
        O.PCARE1 = unname(D.poinc[1]),
        O.PCARE2 = unname(D.poinc[2]),
        O.MEAN = D.mean,
        O.MDN = D.median,
        O.MIN = D.min,
        O.MAX = D.max,
        O.SDEV = D.sdev,
        O.MLE = D.mle,
        O.HURST1 = D.hurst1,
        O.HURST2 = D.hurst2,
        O.BP = D.box,
        O.P05 = D.qt05,
        O.P95 = D.qt95,
        O.IQR = D.iqr,
        O.SLP = D.slp,
        O.NORM = D.norm,
        O.PACF = D.pacf,
        O.ACF = D.acf,
        O.NO = D.nout2,
        O.AMP = D.strg,
        O.TP = D.tp,
        O.STEP = D.step,
        O.PEAKD = unname(D.npeak[1]),
        O.PEAKI = unname(D.npeak[2])
      )

    DYNAMICS <- round(DYNAMICS,3)

    DYNAMICS[is.infinite(DYNAMICS)] <- NA_real_

    DYNAMICS
  }


#' Get fourieer terms for test
#'
#' @param y time series
#' @param nterms nterms
#' @param freq frequency
#' @param h horizon
#'
#' @keywords internal
#' @import forecast
#'
#' @export
get_fourier_terms_ <-
  function(y, nterms, freq, h) {
    #require(forecast)
    y <- ts(y, frequency = freq)

    fterms <- fourier(y, K = nterms, h=h)

    colnames(fterms) <- paste0("DHR.F", 1:ncol(fterms))
    fterms <- as.data.frame(fterms)

    fterms
  }


