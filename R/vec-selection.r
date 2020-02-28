#' Feature Selection, best summary by rep
#'
#' @param x object of class VEST
#'
#' @export
feat_select_vest <-
  function(x) {
    vi <-
      lapply(x@importance,
             function(h_vi) {
               h_vi$RReliefFequalK
             })

    vi <- rowMeans(as.data.frame(vi))
    vi_positive <- vi[vi > 0]#beta=0

    nms <- names(vi_positive)
    stats.ids <- grep("^tm", nms)
    dyn_nms <- nms[-stats.ids]
    vi_dyns <- vi_positive[-stats.ids]

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



#' Feature Selection, best rep
#'
#' @param x object of class VEST
#'
#' @export
feat_select_br <-
  function(x) {

    vi <-
      lapply(x@importance,
             function(h_vi) {
               h_vi$RReliefFequalK
             })

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

