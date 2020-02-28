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

    cat("Transform: WINS\n")
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
        T_WINS = T_WINS,
        T_BC = T_BC)

    keys <- list(k_scale = scale_keys, lambda = lambda)

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
#' @export
ft_t_wins <-
  function(x) {
    n <- floor(sqrt(ncol(x)))

    xs <-
      apply(x, 1, function(ev) {
        trim.ids <- order(abs(ev), decreasing = TRUE)[1:n]
        ev[trim.ids] <- stats::median(ev)
        ev
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



