#' Importance Scores
#'
#' @param form formula
#' @param x dataset
#'
#' @import CORElearn
#'
#' @export
estimate_fi <-
  function(form, x) {
    fi_methods <- c("RReliefFequalK",
                    "MSEofMean",
                    #"MSEofModel",
                    "RReliefFwithMSE"
                    #"RReliefFsqrDistance"
    )

    FI <-
      lapply(fi_methods,
             function(m) {
               cat(m,"\n")
               round(attrEval(form, x, estimator = m), 5)
             })

    #FI[[1]] <- rank(-FI[[1]])
    #FI[[2]] <- rank(-FI[[2]])
    #FI[[3]] <- rank(-FI[[3]])

    names(FI) <- fi_methods

    FI
  }

