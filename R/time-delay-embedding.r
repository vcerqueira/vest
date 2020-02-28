#' Search best embedding dimension
#' using model tree performance
#'
#' @param x time series, vector
#' @param max_k maximum embedding dimension. Def to 30
#'
#' @export
search_k_vall <-
  function(x, max_k=30) {
    ksearch <- 10:max_k
    kresults <- numeric(length(ksearch))

    for (in_k in 1:length(ksearch)) {
      k_ <- ksearch[in_k]
      xk_ <- my_embedd(x, m = k_, d = 1)
      kresults[in_k] <- M5.self_cycle(target ~., xk_, iterations=1)
    }
    VALLOSS_K <- ksearch[which.min(kresults)]

    VALLOSS_K
  }


#' Embedding function
#'
#' @param x time series, vector
#' @param m embedding dimension, int
#' @param d delay parameter, def to 1
#' @param h forecasting horizon, def to 1
#'
#' @import tseriesChaos
#'
#' @export
my_embedd <-
  function(x,m,d=1,h=1) {
    xk <- embedd(x, m = m, d = d)
    n <- ncol(xk)

    if (h < 2) {
      colnames(xk) <- paste0("tm",(n-1):0)
      colnames(xk)[n] <- "target"
    } else {
      colnames(xk)[(n-h+1):n] <- paste0("target",1:h)
      colnames(xk)[-c((n-h+1):n)] <- paste0("tm",(n-h):1)
    }

    as.data.frame(xk)
  }
