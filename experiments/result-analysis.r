load("/Users/vcerqueira/Desktop/VEST_PT2_LASSO_30092020.rdata")
load("/Users/vcerqueira/Desktop/VEST_PT2_M5_30092020.rdata")

source("experiments/analysis-src.r")
#source("analysis-src.r")

library(vest)
library(ggplot2)
library(reshape2)

n <- length(FINALRESULTS)

Lmase <- vector("list", n)
for (i in 1:length(FINALRESULTS)) {
  cat(i,"/",n,"\n")
  #i<-1
  x <- FINALRESULTS[[i]][[1]]

  names(x$yhat) <-
    c("AR", "VEST", "AR+BS", "AR+VEST", "AR+BT",
      "ARIMA","ETS","TBATS")

  frq <- x$feats_model@keys$freq

  Lmase[[i]] <-
    lapply(x$yhat,
           function(z) {
             #cat("-")
             err_by_h_mase(
               yhat = z,
               y = as.matrix(x$y[,1:2]),
               y_tr = x$y_tr,
               frq = frq
             )
           })
}

L <-
  lapply(Lmase, function(err) {
  lapply(err, function(z) {
    z[-6]
  })
})

L_np <- lapply(L, function(x) sapply(x, function(z) z[["L_np"]]))
L_final <- L_np

sort(bind_and_avgrank(L_final, NULL)[[1]])
rank_all <- bind_and_avgrank(L_final, 1:length(L_final[[1]]))
avg_rank_plot(rank_all[[1]], rank_all[[2]]) +
  theme(axis.text.x  = element_text(angle = 0,size = 11))

#
benchmark <- "AR+VEST"
angl <- 0
xresults <- bayes_analysis(L_final, benchmark, rope = 2.5)[-4,]
colnames(xresults) <-
  c("AR+VEST loses", "draw", "AR+VEST wins")
bayes_plot(x = xresults) +
  theme(axis.text.x  = element_text(angle = angl, size = 10, h=.5)) +
  scale_fill_brewer(palette="PuBuGn")


###
