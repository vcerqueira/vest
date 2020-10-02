load("/Users/vcerqueira/Desktop/VEST_PT1_25092020.rdata")
source("experiments/analysis-src.r")
library(reshape2)
library(ggplot2)

importance_values <-
  sapply(RESULTS,
         function(x) {
           x[[1]]$feature_model@importance$target1
         })

importance_values <- complete_list(importance_values)

importance_values <-
  lapply(importance_values,
         function(x) {
           x[order(names(x))]
         })

importance_values <- as.data.frame(importance_values)
importance_values <- t(importance_values)

colnames(importance_values) <-
  gsub("^tm","LAG.",colnames(importance_values))

##

importance_rank <- apply(-importance_values, 1, rank)

top<-order(apply(importance_rank,1,median))[c(2,1,3:30)]
bot<-order(apply(importance_rank,1,median), decreasing = T)[1:30]

bp_dist(t(importance_rank[c(top,bot),])) +
  theme(axis.text.x  = element_text(size=8,
                                    angle=90))

##


codes <- vapply(colnames(importance_values),
                split_by., character(2))

codes <- codes[2,-grep("^LAG", colnames(codes))]

cn_without_lags = colnames(importance_values)
cn_without_lags<-cn_without_lags[-grep("^LAG",cn_without_lags)]


feat_imp_by_stat <- split(cn_without_lags,unname(codes))


feat_imp_by_stat_df <-
  as.data.frame(lapply(feat_imp_by_stat,
                       function(repre) {
                         apply(importance_values[, repre], 1, median,na.rm = T)
                       }))

importance_rank <- apply(-feat_imp_by_stat_df, 1, rank)
bp_dist(t(importance_rank)) +
  theme(axis.text.x  = element_text(size=10,
                                    angle=90))



#


codes <- vapply(colnames(importance_values),
                split_by., character(2))

feat_imp_by_rep <- split(colnames(importance_values),unname(codes[1,]))


feat_imp_by_rep_df <-
  as.data.frame(lapply(feat_imp_by_rep,
                       function(repre) {
                         apply(importance_values[, repre], 1, mean,na.rm = T)
                       }))

importance_rank <- apply(-feat_imp_by_rep_df, 1, rank)
bp_dist(t(importance_rank)) +
  theme(axis.text.x  = element_text(size=10,
                                    angle=90))



#


