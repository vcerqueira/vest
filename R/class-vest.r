setClassUnion("numORdf", c("numeric", "data.frame"))

#' Class for time series feature eng.
#'
#' @slot X Predictor variables. Past recent values
#' @slot y target values. numeric if h=1, df if h>1
#' @slot Z list of representations of X
#' @slot Dynamics statistics derived from Z and X
#' @slot keys metadata
#' @slot importance importance scores of features
#'
#' @export
setClass("VEST",
         slots = c(X = "data.frame",
                   y = "numORdf",
                   Z = "list",
                   Dynamics = "data.frame",
                   keys = "list",
                   importance = "list")
)

#' Data.frame constructor for VEST class
#'
#' @param targets character vector describing the target variable(s)
#' @param x embedded time series as data.frame
#'
#' @export
VEST <-
  function(targets, x) {
    tgti <- which(colnames(x) %in% targets)

    X <- x[,-tgti]
    y <- x[,tgti]

    methods::new(
      "VEST",
      X = X,
      y = y,
      Z = list(),
      Dynamics = data.frame(),
      keys = list(),
      importance = list()
    )
  }


###########
### METHODS
###########

#' Get transformations generic fun
#'
#' @param object object of class VEST
#' @keywords internal
#'
#' @export
setGeneric("get_transformations",
           function(object) {
             standardGeneric("get_transformations")
           })

#' Transform the original representation
#'
#' @param object object of class VEST
#' @keywords internal
#' @export
setMethod("get_transformations",
          signature("VEST"),
          function(object) {
            freq <- object@keys[["freq"]]

            FT_TRANS <- ft_transformation(object@X, freq)

            object@Z <- FT_TRANS$tlist
            object@keys <- c(object@keys, FT_TRANS$keys)

            object
          })

#' Predict feature values with new observations
#'
#' @param object object of class VEST
#' @param x x past lags w/o targets
#'
#' @export
setMethod("predict",
          signature("VEST"),
          function(object, x, update_last_rows=FALSE) {
            object@X <- x
            object@Dynamics <- data.frame()
            object@y <- data.frame()

            object <- apply_transformations(object)
            #sapply(object@Z, dim)
            #object@keys$last_freq_x

            object <- get_summary(object)
            inner.dynamics <- object@Dynamics

            #outer.dynamics <- predict_outer_dynamics(object, x)
            #outer.dynamics <- tail(outer.dynamics, nrow(inner.dynamics))

            DYNS <- inner.dynamics#cbind.data.frame(inner.dynamics, outer.dynamics)
            # upd last rows

            if (update_last_rows) {
              print("updating last rows")
              object@keys$last_freq_x <-
                c(object@keys$last_freq_x,
                  head(x,1)[,"tm1"])

              object@keys$last_freq_x <- tail(object@keys$last_freq_x, -1)

              ##
              object@keys$past_x <-
                c(object@keys$past_x,
                  head(x,1)[,"tm1"])

              object@keys$past_x <- tail(object@keys$past_x, -1)


              object@keys$past_l <-
                c(object@keys$past_l,
                  head(x,1)[,"tm1"])

              object@keys$past_l <- tail(object@keys$past_l, -1)
            }


            bf <- object@keys$bad_feats
            if (length(bf) > 0) {
              DYNS <- DYNS[,-bf]
            }

            if (any(sapply(DYNS, is.infinite))) {
              DYNS <- replace_inf(DYNS)
            }

            dynnm <- colnames(DYNS)
            cvals <- object@keys$typical_vals
            for (i in 1:ncol(DYNS)) {
              if (any(is.na(DYNS[,i]))) {
                DYNS[,i][is.na(DYNS[,i])] <- cvals[[dynnm[i]]]
              }
            }

            DYNS
          })


#' Apply transformations generic
#'
#' @param object object of class VEST
#' @keywords internal
#'
#' @export
setGeneric("apply_transformations",
           function(object) {
             standardGeneric("apply_transformations")
           })

#' Apply transformations using VEST
#'
#' @param object object of class VEST
#'
#' @export
setMethod("apply_transformations",
          signature("VEST"),
          function(object) {
            nms <- names(object@Z)

            if (length(nms) == 0) {
              stop("No transformation available.")
            }



            TLIST <-
              lapply(nms,
                     function(o) {
                       switch (o,
                               "T_SCL" = {
                                 ft_t_scale(object@X,
                                            center = object@keys$k_scale[["center"]],
                                            scale = object@keys$k_scale[["scale"]])
                               },
                               "T_COS" = {
                                 ft_t_fourier_cos(object@X,
                                                  freq = object@keys$freq,
                                                  past_l = object@keys$past_l)
                               },
                               "T_SIN" = {
                                 ft_t_fourier_sin(object@X,
                                                  freq = object@keys$freq,
                                                  past_l = object@keys$past_l)
                               },
                               "T_DWT" = {
                                 ft_t_dwt(object@X)
                               },
                               "T_WINS" = {
                                 ft_t_wins(object@X)
                               },

                               "T_DIFF" = {
                                 ft_t_diff(object@X)
                               },
                               "T_DIFF2" = {
                                 ft_t_diff2(object@X)
                               },
                               "T_BC" = {
                                 ft_t_boxcox(object@X,
                                             lambda = object@keys$lambda)$x
                               },
                               "T_SMA" = {
                                 ft_t_sma(object@X)
                               },
                               NULL)
                     })

            names(TLIST) <- nms

            object@Z <- TLIST

            object
          })

#' Feature Engineering Wrapper
#'
#' @param x Embedded time series
#' @param targets target column names
#' @param freq frequency of the time series
#' @param compute_importance compute_importance
#'
#' @export
feature_engineering <-
  function(x, targets, freq, compute_importance=TRUE) {
    cat("Creating class object\n")
    xobj <- VEST(targets, x)
    cat("Transform...\n")
    xobj@keys[["freq"]] <- freq
    xobj <- get_transformations(object = xobj)
    cat("Summarise\n")
    xobj <- get_summary(object = xobj)
    #cat("Get outer dynamics\n")
    #xobj <- get_outer_dynamics(object = xobj)
    cat("Cleanup\n")
    xobj <- cleanup_feats(object = xobj)

    if (compute_importance) {
      cat("Variable Importance\n")
      xobj <- get_importance(xobj)
    }

    xobj
  }


