#' Importance Scores
#'
#' @param form formula
#' @param x dataset
#'
#' @export
estimate_fi <-
  function(form, x) {

    fi <- CORElearn::attrEval(form, x,
                              estimator = "RReliefFequalK")

    fi
  }



#' Bayes Sign Test
#'
#' @param diffVector diffVector
#' @param rope_min rope_min
#' @param rope_max rope_min
#'
#' @export
BayesianSignTest <- function(diffVector,rope_min, rope_max) {
  #rope_min<-rope[1]
  #rope_max<-rope[2]

  library(MCMCpack)

  samples <- 3000

  #build the vector 0.5 1 1 ....... 1
  weights <- c(0.5,rep(1,length(diffVector)))

  #add the fake first observation in 0
  diffVector <- c (0, diffVector)


  #for the moment we implement the sign test. Signedrank will follows
  probLeft <- mean (diffVector < rope_min)
  probRope <- mean (diffVector > rope_min & diffVector < rope_max)
  probRight <- mean (diffVector > rope_max)

  r <- rdirichlet(samples, c(probLeft,probRope,probRight))
  r <- colMeans(r)

  results = c("probLeft"=r[1], "probRope"=r[2],"probRight"=r[3])

  return (results)

}



#' Get target vector
#'
#' @param form formula
#' @param x data set, data.frame
#'
#' @export
get_y_val <-
  function(form, x) {
    stats::model.response(stats::model.frame(form, x, na.action = NULL))
  }

#' Unroll embedded time series
#'
#' @param df time series, data.frame (my_embedd fun)
#'
#' @export
unroll_embedd <-
  function(df) {
    pt1 <- unlist(df[1,-ncol(df)], use.names = FALSE)
    pt2 <- df[,ncol(df)]
    c(pt1, pt2)
  }

#' Create train/test data sets
#'
#' @param train train series, vector
#' @param test test series, vector, posterior to train
#' @param k embedding dimension
#' @param h forecasting horizon
#'
#' @export
create_datasets <-
  function(train, test, k, h) {
    train <- as.numeric(train)
    test <- as.numeric(test)

    tr_k <- my_embedd(train, m = k, d = 1, h = h)

    tr_tail <- utils::tail(train, k-1)

    ts_k <- my_embedd(c(tr_tail,test), m = k, d = 1, h = h)

    list(train=tr_k,test=ts_k)
  }


#' Holdout estimation
#'
#' @param x time series, ts class
#' @param ratio ratio for training
#' @param frq frequency of time series
#'
#' @export
ts_holdout <-
  function(x, ratio, frq) {
    len <- NROW(x)

    train <- head(x, ceiling(ratio * len))
    test <- tail(x, len - ceiling(ratio * len))

    train <- ts(train, frequency = frq)
    test <- ts(test, frequency = frq)

    stopifnot(is.ts(train))
    stopifnot(is.ts(test))

    list(train=train,test=test)
  }


#' holdout
#'
#' @param x x
#' @param beta bear
#'
#' @export
myholdout2 <- function(x, beta) {
  len <- NROW(x)
  train <- x[ seq_len(beta * len), ]
  test <-  x[-seq_len(beta * len), ]

  list(train=train, test=test)
}



#' replace infs
#'
#' @param df data.frame
#'
#' @export
replace_inf <- function(df) {
  do.call(data.frame,
          lapply(df, function(j) {
            replace(j, is.infinite(j), NA)
          })
  )
}


#' log transform
#'
#' @param x numeric vec
#' @export
log_trans <- function(x) sign(x) * log(abs(x) + 1)

#' normalize
#'
#' @param x num vec
#' @export
proportion <- function(x) x / sum(x, na.rm = T)

#' perc difference
#'
#' @param x num vec 1
#' @param y num vec 2
#'
#' @export
percentual_difference <-
  function(x,y) {
    ((x - y) / abs(y)) * 100
  }

#' Splitting data
#'
#' @param x dataset, data.frame
#' @param test_size test size, no of obs.
#'
#' @export
ts_df_slip_by_size <-
  function(x, test_size) {
    test <- tail(x, test_size)
    train <- head(x, nrow(x) - test_size)

    list(train=train,test=test)
  }


#' Embed dim. estimation
#'
#' @param x time series, ts class
#'
#' @export
K_HAT <-
  function(x) {
    frq <- stats::frequency(x)

    xs <- ts_holdout(x, .8, frq = frq)

    cat("Estimating best k\n")
    k <- search_k_vall(xs$train, max_k = 30)

    k
  }

split_by. <- function(expr, ...) split_by(expr, split = ".", ...)

split_by <- function(expr, split, unlist. = TRUE, ...) {
  expr <- strsplit(expr, split = split, fixed = TRUE, ...)
  if (unlist.) expr <- unlistn(expr)
  expr
}

#' Soft completion
#'
#' @param x data.frame with NAs
#' @import softImpute
#' @keywords internal
#' @export
soft_completion <- function(x) {
  cls <- class(x)

  if ("data.frame" %in% cls) {
    x <- as.matrix(x)
  }

  impModel <- softImpute(x)

  xp <- complete(x, impModel)

  if ("data.frame" %in% cls) {
    xp <- as.data.frame(xp)
  }

  list(x=xp, model=impModel)
}

unlistn <- function(x) unlist(x,use.names = F)

#' rep h origins
#'
#' @param n len
#' @param nreps n sims
#' @param train_size ratio of len
#' @param test_size ratio of len
#'
#' @export
rep_holdout_origins <-
  function(n, nreps, train_size, test_size) {
    tr_size <- as.integer(n * train_size)
    ts_size <- as.integer(n * test_size)
    selection_range <- (tr_size + 1):(n - ts_size + 1)
    origins <- sample(selection_range, nreps)

    origins
  }
