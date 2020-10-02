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

            suppressWarnings(rm_NA <- manyNAs(t(object@Dynamics), .7))

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
