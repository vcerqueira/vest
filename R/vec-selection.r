#' Feature Selection, best summary by rep2
#'
#' @param x object of class VEST
#'
#' @export
feat_select_bf <-
  function(x) {
    require(caret)
    vi <- x@importance


    idx <- findCorrelation(cor(x@Dynamics), 0.95, exact = F)

    uncorr_dyns <- names(x@Dynamics)[-idx]

    vi <- as.data.frame(vi)
    vi_positive <- rowMeans(vi)
    vi_positive <- vi_positive[vi_positive>0]

    vi_positive <- vi_positive[names(vi_positive) %in% uncorr_dyns]

    nms <- names(vi_positive)
    stats.ids <- grep("^tm", nms)

    if(length(stats.ids) > 0 ) {
      dyn_nms <- nms[-stats.ids]
      vi_dyns <- vi_positive[-stats.ids]
    } else {
      dyn_nms <- nms
      vi_dyns <- vi_positive
    }

    codes <- vapply(dyn_nms, split_by., character(2))

    feat_imp_by_stat <- split(vi_dyns,unname(codes[2,]))

    best_by_stat <-
      lapply(feat_imp_by_stat,
             function(z) {
               sort(z, decreasing = TRUE)[1]
             })

    top_feats <- unlist(unname(best_by_stat))

    chosen_feats <- names(top_feats)

    dyncols <- colnames(x@Dynamics)

    poor_feats <- which(!dyncols %in% chosen_feats)

    chosen_DYNS <- x@Dynamics[,-poor_feats,drop=FALSE]

    chosen_DYNS
  }


#' Feature Selection, best summary by rep3
#'
#' @param x object of class VEST
#'
#' @export
feat_select_corr <-
  function(x) {
    require(caret)


    idx <- findCorrelation(cor(x@Dynamics), 0.95, exact = F)

    chosen_feats <- names(x@Dynamics)[-idx]

    dyncols <- colnames(x@Dynamics)

    poor_feats <- which(!dyncols %in% chosen_feats)

    chosen_DYNS <- x@Dynamics[,-poor_feats,drop=FALSE]

    chosen_DYNS
  }


#' Feature Selection, best rep
#'
#' @param x object of class VEST
#'
#' @export
feat_select_br <-
  function(x) {

    vi <- x@importance

    vi <- rowMeans(as.data.frame(vi))

    ids.lags <- grep("^tm", names(vi))
    vi.lags <- vi[ids.lags]
    avg.vi.lags <- mean(vi.lags)
    vi.nonlags <- vi[-ids.lags]
    v.nms <- names(vi.nonlags[vi.nonlags > 0])
    abavg.ids <- which(names(vi) %in% v.nms)
    all.ids <- c(ids.lags,abavg.ids)

    vi_positive <- vi[all.ids]

    nms <- names(vi_positive)
    stats.ids <- grep("^tm", nms)
    dyn_nms <- nms[-stats.ids]
    vi_dyns <- vi_positive[-stats.ids]

    codes <- vapply(dyn_nms, split_by., character(2))

    feat_imp_by_rep <- split(vi_dyns,unname(codes[1,]))
    sum_by_rep <- sapply(feat_imp_by_rep, sum)
    best_rep <- names(sum_by_rep[which.max(sum_by_rep)])

    top_feats <- names(feat_imp_by_rep[[best_rep]])

    dyncols <- colnames(x@Dynamics)

    poor_feats <- which(!dyncols %in% top_feats)

    chosen_DYNS <- x@Dynamics[,-poor_feats,drop=FALSE]

    chosen_DYNS
  }
