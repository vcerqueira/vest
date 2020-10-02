#' Feature transformations
#'
#' @param x Predictor variables of embedded time series
#' @param freq frequency of time series
#'
#' @export
ft_transformation <-
  function(x, freq) {
    scale_keys <- get_scale_keys(y = x[,ncol(x)])

    cat("Transform: DWT\n")
    T_DWT <- ft_t_dwt(x)

    cat("Transform: DIFF\n")
    T_DIFF <- ft_t_diff(x)

    cat("Transform: DIFF2\n")
    T_DIFF2 <- ft_t_diff2(x)

    cat("Transform: SMA\n")
    T_SMA <- ft_t_sma(x)

    cat("Transform: WIN\n")
    T_WINS <- ft_t_wins(x)

    cat("Transform: BoxCox\n")
    T_BC <- ft_t_boxcox(x, lambda = NULL)
    lambda <- T_BC$lambda
    T_BC <- T_BC$x

    transformation_list <-
      list(
        T_DWT = T_DWT,
        T_DIFF = T_DIFF,
        T_DIFF2=T_DIFF2,
        T_SMA = T_SMA,
        T_BC = T_BC,
        T_WINS=T_WINS)

    keys <- list(k_scale = scale_keys,
                 lambda = lambda)

    list(keys = keys, tlist = transformation_list)
  }

#' Get scaling factors
#'
#' @param y num vec
#'
#' @export
get_scale_keys <-
  function(y) {
    avg <- mean(y)
    sdev <- sd(y)

    c(center=avg, scale=sdev)
  }

#' First Differences transf.
#'
#' @param x df
#'
#' @export
ft_t_diff <-
  function(x) {
    xs <- apply(x, 1, diff)

    xs <- t(xs)

    if (is.data.frame(x)) {
      xs <- as.data.frame(xs)
    }

    colnames(xs) <- paste0("DIFF",0:(ncol(xs)-1))
    xs
  }


#' First Differences transf.asdada
#'
#' @param x df
#'
#' @export
ft_t_fourier_sin <-
  function(x, freq, past_l) {
    #require(vest)
    #x <- rnorm(100)
    #x <- my_embedd(x,10)
    k.x <- ncol(x)
    x.ur0 <- x.ur <- unroll_embedd(x)

    if (!is.null(past_l)) {
      dummy_y <- rep(0, times=past_l)
      x.ur <- c(dummy_y, x.ur)
    }

    x.ur <- ts(x.ur, frequency = freq)

    f_transf <- fourier(x.ur, K=1)
    sin_curve <- f_transf[,1]

    sin_curve <- tail(sin_curve, length(x.ur0))

    x.rr <- my_embedd(sin_curve, m=k.x, 1, 1)
    x.rr <- x.rr[,ncol(x.rr):1]

    colnames(x.rr) <- paste0("FOURS",0:(ncol(x.rr)-1))

    x.rr
  }


#' First Differsssaassadences transf.
#'
#' @param x df
#'
#' @export
ft_t_fourier_cos <-
  function(x, freq, past_l) {
    #require(vest)
    #x <- rnorm(100)
    #x <- my_embedd(x,10)
    k.x <- ncol(x)
    x.ur0 <- x.ur <- unroll_embedd(x)

    if (!is.null(past_l)) {
      dummy_y <- rep(0, times=past_l)
      x.ur <- c(dummy_y, x.ur)
    }

    x.ur <- ts(x.ur, frequency = freq)

    f_transf <- fourier(x.ur, K=1)
    sin_curve <- f_transf[,2]

    sin_curve <- tail(sin_curve, length(x.ur0))

    x.rr <- my_embedd(sin_curve, m=k.x, 1, 1)
    x.rr <- x.rr[,ncol(x.rr):1]

    colnames(x.rr) <- paste0("FOURC",0:(ncol(x.rr)-1))

    x.rr
  }


#' Second Differences transf.
#'
#' @param x df
#'
#' @export
ft_t_diff2 <-
  function(x) {
    xs <- apply(x, 1, diff, differences = 2)

    xs <- t(xs)

    if (is.data.frame(x)) {
      xs <- as.data.frame(xs)
    }

    colnames(xs) <- paste0("DIFF2",0:(ncol(xs)-1))
    xs
  }


#' DWT transf.
#'
#' @param x df
#'
#' @import wmtsa
#'
#' @export
ft_t_dwt <-
  function(x) {
    xs <-
      apply(x, 1, function(ev) {
        dwt_ <- wavDWT(ev, wavelet = "s8", n.levels = 2)
        dwt_$data$d1
      })

    xs <- t(xs)

    if (is.data.frame(x)) {
      xs <- as.data.frame(xs)
    }

    colnames(xs) <- paste0("DWT",0:(ncol(xs)-1))

    xs
  }


#' SMA transf.
#'
#' @param x df
#'
#' @import TTR
#'
#' @export
ft_t_sma <-
  function(x) {
    n <- floor(sqrt(ncol(x)))

    xs <-
      apply(x, 1, function(ev) {
        if (length(ev[!(is.na(ev) | is.infinite(ev))]) > 0) {
          ev[is.na(ev) | is.infinite(ev)] <-
            stats::median(ev[!(is.na(ev) | is.infinite(ev))])
        } else {
          ev[is.na(ev) | is.infinite(ev)] <- -1
        }

        sma_ <- SMA(ev, n)
        sma_[!is.na(sma_)]
      })

    xs <- t(xs)

    if (is.data.frame(x)) {
      xs <- as.data.frame(xs)
    }

    colnames(xs) <- paste0("SMA",0:(ncol(xs)-1))

    xs
  }



#' Winsorization transf.
#'
#' @param x df
#'
#' @import DescTools
#'
#' @export
ft_t_wins <-
  function(x) {
    n <- floor(sqrt(ncol(x)))

    xs <-
      apply(x, 1, function(ev) {
        Winsorize(ev)
      })

    xs <- t(xs)

    if (is.data.frame(x)) {
      xs <- as.data.frame(xs)
    }

    colnames(xs) <- paste0("WINS",0:(ncol(xs)-1))

    xs
  }


#' Boxcox
#'
#' @param x df
#' @param lambda lambda
#'
#' @export
ft_t_boxcox <-
  function(x, lambda=NULL) {
    k.x <- ncol(x)
    x.ur <- unroll_embedd(x)

    if (is.null(lambda)) {
      lambda <- BoxCox.lambda(x.ur)
    }

    x.bc <- BoxCox(x.ur, lambda)
    x.rr <- my_embedd(x.bc, m=k.x, 1, 1)

    colnames(x.rr) <- paste0("BC",0:(ncol(x.rr)-1))

    list(x=x.rr, lambda=lambda)
  }



#' Seasonal differencing
#'
#' @param x df
#' @param freq freq
#' @param last_freq_x last_freq_x
#'
#' @export
ft_t_sdiff <-
  function(x, freq, last_freq_x = NULL) {

    k.x <- ncol(x)
    x.ur <- unroll_embedd(x)

    surr_x <- ts(x.ur, frequency = freq)

    if (!is.null(last_freq_x)) {
      surr_x <- c(last_freq_x, surr_x)
    }

    dx <- diff(surr_x, lag = freq)
    dx <- tail(dx, length(x.ur))

    dlen <- length(x.ur) - length(dx)
    if (dlen > 0) {
      dx <- c(rep(NA_real_, times = dlen), dx)
    }

    x.rr <- my_embedd(dx, m=k.x, 1, 1)

    colnames(x.rr) <- paste0("SD",0:(ncol(x.rr)-1))

    list(x=x.rr, last_freq_x=tail(x.ur, freq))
  }



#' Seasonal adjustment
#'
#' @param x df
#' @param freq freq
#' @param past_x last_freq_x
#'
#' @export
ft_t_seasadj <-
  function(x, freq, past_x = NULL) {

    k.x <- ncol(x)
    nx <- nrow(x)
    x.ur <- unroll_embedd(x)

    if (!is.null(past_x)) {
      x.ur <- c(past_x, x.ur)
    }

    surr_x <- ts(x.ur, frequency = freq)

    xdecomp <- forecast::mstl(surr_x)

    xadj <- forecast::seasadj(xdecomp)

    xadj <- tail(xadj, length(x.ur))

    dlen <- length(x.ur) - length(xadj)
    if (dlen > 0) {
      xadj <- c(rep(NA_real_, times = dlen), xadj)
    }

    x.rr <- my_embedd(xadj, m=k.x, 1, 1)

    colnames(x.rr) <- paste0("SA",0:(ncol(x.rr)-1))

    x.rr <- tail(x.rr, nx)

    list(x=x.rr, past_x=x.ur)
  }


#' Seasonal component
#'
#' @param x df
#' @param freq freq
#' @param past_x last_freq_x
#'
#' @export
ft_t_seascomp <-
  function(x, freq, past_x = NULL) {

    k.x <- ncol(x)
    nx <- nrow(x)
    x.ur <- unroll_embedd(x)


    if (!is.null(past_x)) {
      x.ur <- c(past_x, x.ur)
    }

    surr_x <- ts(x.ur, frequency = freq)

    xdecomp <- forecast::mstl(surr_x)

    xadj <-
      xdecomp[,grep("Seaso", colnames(xdecomp))]

    xadj <- tail(xadj, length(x.ur))

    dlen <- length(x.ur) - length(xadj)
    if (dlen > 0) {
      xadj <- c(rep(NA_real_, times = dlen), xadj)
    }

    x.rr <- my_embedd(xadj, m=k.x, 1, 1)

    colnames(x.rr) <- paste0("SEC",0:(ncol(x.rr)-1))

    x.rr <- tail(x.rr, nx)

    list(x=x.rr, past_x=x.ur)
  }



