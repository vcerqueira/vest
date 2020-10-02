#' Relative dispersion
#'
#' @param x num vec
#'
#' @export
relative_dispersion <-
  function(x) {
    sd(x) / sd(diff(x)[-1])
  }


#' Max lyapunov exponent
#'
#' @param x time series vec
#'
#' @export
max_lyapunov_exp <-
  function(x) {
    #require(nonlinearTseries)

    len <- length(x)
    Reduce(max,
           nonlinearTseries::divergence(
             nonlinearTseries::maxLyapunov(
               time.series = x,
               min.embedding.dim = 1,
               max.embedding.dim = len,
               radius = ceiling(sqrt(len)),
               do.plot = FALSE
             )
           ))
  }

#' Hurst exponent
#'
#' @param x numeric vector
#' @param nmoments no moments
#'
#' @import Rwave
#'
#' @export
HURST <-
  function(x, nmoments=1) {
    #require(Rwave)

    cwtwnoise <- DOG(x, 10, 5, nmoments, plot = FALSE)
    mcwtwnoise <- Mod(cwtwnoise)
    mcwtwnoise <- mcwtwnoise * mcwtwnoise
    wspwnoise <- tfmean(mcwtwnoise, plot = FALSE)

    hurst.est(wspwnoise, 1:5, 5, plot = FALSE)[[2]]
  }


#' time series acceleration
#'
#' @param x num vec
#'
#' @export
accelaration <-
  function(x) {
    #require(TTR)

    if (length(x) > 10) {
      n <- 5
    }
    else {
      n <- 3
    }

    if (length(x) < 4) {
      return(
        c(avg_accl=NA,
          sdev_accl=NA)
      )
    }

    ts_sma <- TTR::SMA(x, n = n)
    ts_ema <- TTR::EMA(x, n = n)

    ts_sma <- ts_sma[!is.na(ts_sma)]
    ts_ema <- ts_ema[!is.na(ts_ema)]

    sema <- ts_sma / ts_ema
    sema <- sema[!(is.infinite(sema) | is.na(sema))]

    accl_ <- ts_sma / ts_ema

    c(avg_accl=mean(accl_),
      sdev_accl=stats::sd(accl_))
  }

#' Slope
#'
#' @param x num vec
#'
#' @export
slope <-
  function(x) {
    time_x <- seq_along(x)
    lmfit <- lm(x ~ time_x)

    lmfit$coefficients[[2]]
  }


#' Daubechies DWT
#'
#' @param x num vec
#' @param lvl level of transformation
#'
#' @import wmtsa
#'
#' @export
dauDWTenergy <-
  function(x, lvl=4) {

    r <-
      tryCatch(wavDWT(x, wavelet="s8", n.levels=lvl),
               error = function(e) {
                 NA
               })

    if (is.na(r)) {
      dummy_ed <- rep(NA, times=lvl)
      names(dummy_ed) <- paste0("Ed",1:lvl)

      return(c(E_ra5=NA,dummy_ed))
    }

    dwt_result <- r$data

    E_a5 <- reconstruct(r)
    E_a5 <- norm(t(E_a5))

    E_dk <- sapply(dwt_result[1:lvl], function(x) norm(t(x)))
    names(E_dk) <- paste0("Ed", 1:length(E_dk))

    E_T <- E_a5 + sum(E_dk)

    E_ra5 <- E_a5 / E_T

    E_rdk <- E_dk / E_T

    c(E_ra5=E_ra5,E_rdk)
  }


#' no outliers
#'
#' @param x num vec
#'
#' @export
nout2 <-
  function(x) {
    detr <- diff(x)

    sum(abs(detr) > 1.5*IQR(detr))
  }

#' Step change
#'
#' @param x num vec
#'
#' @export
step_change <-
  function(x) {
    lko <- x[length(x)]
    rv <- x[-length(x)]

    has_s_c <- abs(lko - mean(rv)) > 2*sd(rv)

    as.integer(has_s_c)
  }

#' N peaks
#'
#' @param x num vec
#'
#' @export
npeaks <-
  function(x) {
    nincr <- sum(diff(sign(diff(x)))==-2)
    ndecr <- sum(diff(sign(diff(x)))==2)
    l <- length(x)

    c(ndecr=ndecr/l, nincr=nincr/l)
  }

#' Turning Points
#'
#' @param x num vec
#'
#' @export
tpoints <-
  function(x) {
    dx <- diff(x)
    sum(sign(dx))
  }

#' Convert FFT
#'
#' http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
#'
#' @param cs complex vals
#'
#' @export
convert.fft <-
  function(cs) {
    sample.rate = 1

    cs <- cs / length(cs) # normalize

    distance.center <- function(c)
      signif(Mod(c),        4)
    angle           <- function(c)
      signif(180 * Arg(c) / pi, 3)

    df <- data.frame(
      cycle    = 0:(length(cs) - 1),
      freq     = 0:(length(cs) - 1) * sample.rate / length(cs),
      strength = sapply(cs, distance.center),
      delay    = sapply(cs, angle)
    )
    df
  }

#' FFT AMP
#'
#' @param x num vec
#'
#' @export
fft_strength <-
  function(x) {
    dx <- diff(x)
    fft_data <- convert.fft(stats::fft(dx))

    mean(fft_data[,"strength"])
  }


#' Poincare variability
#'
#' see RHRV:::computeSD
#'
#' @param x num vec
#'
#' @export
poincare_variability <-
  function(x) {
    sd1 = sd(diff(x))/sqrt(2)
    sd2 = sqrt(2 * var(x) - sd1^2)

    c(sd1=sd1,sd2=sd2)
  }


