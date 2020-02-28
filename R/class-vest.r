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
            object@keys <- FT_TRANS$keys

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
          function(object, x) {
            object@X <- x
            object@Dynamics <- data.frame()
            object@y <- data.frame()

            object <- apply_transformations(object)

            object <- get_summary(object)
            inner.dynamics <- object@Dynamics

            outer.dynamics <- predict_outer_dynamics(object, x)
            outer.dynamics <- tail(outer.dynamics, nrow(inner.dynamics))

            DYNS <- cbind.data.frame(inner.dynamics, outer.dynamics)

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
                               "T_DWT" = {
                                 ft_t_dwt(object@X)
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
                               "T_WINS" = {
                                 ft_t_wins(object@X)
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
#'
#' @export
feature_engineering <-
  function(x, targets, freq) {
    cat("Creating class object\n")
    xobj <- VEST(targets, x)
    cat("Transform...\n")
    xobj <- get_transformations(object = xobj)
    xobj@keys[["freq"]] <- freq
    cat("Summarise\n")
    xobj <- get_summary(object = xobj)
    cat("Get outer dynamics\n")
    xobj <- get_outer_dynamics(object = xobj)
    cat("Cleanup\n")
    xobj <- cleanup_feats(object = xobj)
    cat("Variable Importance\n")
    xobj <- get_importance(xobj)

    xobj
  }

#' Get outer dynamics
#' DHR
#'
#' @param object object of class VEST
#'
#' @export
get_outer_dynamics <-
  function(object) {
    nterms <- 3
    fterms <- get_fourier_terms(object, nterms = nterms)

    object@keys[["y_tr_tm1"]] <- object@X[,"tm1"]
    object@keys[["n_f_terms"]] <- nterms

    outer_dynamics <- fterms

    object@Dynamics <-
      cbind.data.frame(object@Dynamics, outer_dynamics)

    object@keys[["last_p_rows"]] <- tail(object@X, ncol(object@X))

    object
  }

#' Dynamic Harmonic Regre. Terms
#'
#' @param object object of class VEST
#' @param nterms no terms. def to 3
#'
#' @import forecast
#'
#' @export
get_fourier_terms <-
  function(object, nterms=3) {
    x <- object@X

    id <- grep("^tm1$", colnames(x))
    y <- x[,id]

    y <- ts(y, frequency = object@keys$freq)

    fterms <- fourier(y, K = nterms)

    colnames(fterms) <- paste0("DHR.F", 1:ncol(fterms))
    fterms <- as.data.frame(fterms)

    fterms
  }


#' Predicting new DHR terms
#'
#' @param object object of class VEST
#' @param nterms no of terms
#' @param h forecasting horizon
#'
#' @import forecast
#'
#' @export
pred_fourier_terms <-
  function(object, nterms=3, h) {
    y <- object@keys$y_tr_tm1
    y <- ts(y, frequency = object@keys$freq)

    fterms <- fourier(y, K = nterms, h=h)

    colnames(fterms) <- paste0("DHR.F", 1:ncol(fterms))
    fterms <- as.data.frame(fterms)

    fterms
  }


#' Predicting new outer dynamics
#'
#' @param object object of class VEST
#' @param test new observations
#'
#' @export
predict_outer_dynamics <-
  function(object, test) {

    nterms <- object@keys$n_f_terms
    fterms <-
      pred_fourier_terms(object,
                         nterms = nterms,
                         h=nrow(test))


    outer_dynamics <- fterms

    outer_dynamics
  }

