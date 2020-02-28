load("experiments/VEST_pt2_F1_2.rdata")
load("experiments/WF_ARIMA_F1_2.rdata")

source("experiments/analysis-src.r")

library(vest)
library(ggplot2)
library(reshape2)

#names(FINALRESULTS[[1]]$yhat)
ids2rm <- c(1,2,3,5)
n <- length(FINALRESULTS)

Lmase <- vector("list", n)
for (i in 1:length(FINALRESULTS)) {
  cat(i,"/",n,"\n")
  x <- FINALRESULTS[[i]]
  x_arima <- ARIMARESULTS[[i]]

  x$yhat <- c(x$yhat[-ids2rm], list(ARIMA=x_arima))

  frq <- x$feats_model@keys$freq
  if (frq > nrow(x$y)) {
    frq <- 1## for example purposes only!!! usually data is long enough
  }

  Lmase[[i]] <-
    lapply(x$yhat,
           function(z) {
             #cat("-")
             err_by_h_mase(
               yhat = z,
               y = as.matrix(x$y),
               y_tr = x$y_tr,
               frq = frq
             )
           })
}

L_h <-
  lapply(Lmase, function(err) {
    lapply(err, function(z) {
      z$L_h
    })
  })

L <-
  lapply(Lmase, function(err) {
  lapply(err, function(z) {
    z[-6]
  })
})

L_h <-
  lapply(L_h,
         function(x) {
           names(x) <- c("AR+VEST-U", "AR", "VEST",
                         "AR+VEST-FS", "AR+VEST", "AR+BR",
                         "ARIMA"
                         )

           x
         })

L <-
  lapply(L,
         function(x) {
           names(x) <- c("AR+VEST-U", "AR", "VEST",
                         "AR+VEST-FS", "AR+VEST", "AR+BR",
                         "ARIMA")

           x
         })

L_all <- lapply(L, function(x) sapply(x, function(z) z[["L"]]))
L_np <- lapply(L, function(x) sapply(x, function(z) z[["L_np"]]))
L_st <- lapply(L, function(x) sapply(x, function(z) z[["L_st"]]))
L_mt <- lapply(L, function(x) sapply(x, function(z) z[["L_mt"]]))
L_lt <- lapply(L, function(x) sapply(x, function(z) z[["L_lt"]]))


sort(bind_and_avgrank(L_all, NULL)[[1]])
rank_all <- bind_and_avgrank(L_all, 1:length(L_all[[1]]))
avg_rank_plot(rank_all[[1]], rank_all[[2]]) +
  theme(axis.text.x  = element_text(angle = 0,size = 11))

#
benchmark <- "AR"
bid<-2
angl <- 0
bayes_plot(bayes_analysis(L_all, benchmark, rope = 2.5)[-bid, ]) +
  theme(axis.text.x  = element_text(angle = angl, size = 10, h=.5)) +
  scale_fill_brewer(palette="PuBuGn")

NP <- bayes_analysis(L_np,benchmark,  rope = 2.5)[-bid, ]
ST <- bayes_analysis(L_st,benchmark,  rope = 2.5)[-bid, ]
MT <- bayes_analysis(L_mt,benchmark,  rope = 2.5)[-bid, ]
LT <- bayes_analysis(L_lt,benchmark,  rope = 2.5)[-bid, ]

NP <- as.data.frame(t(NP));NP$Horizon <- "Next Point"
ST <- as.data.frame(t(ST));ST$Horizon <- "Short-Term"
MT <- as.data.frame(t(MT));MT$Horizon <- "Medium-Term"
LT <- as.data.frame(t(LT));LT$Horizon <- "Long-Term"

NP$Result <- rownames(NP);rownames(NP)<-NULL
ST$Result <- rownames(ST);rownames(ST)<-NULL
MT$Result <- rownames(MT);rownames(MT)<-NULL
LT$Result <- rownames(LT);rownames(LT)<-NULL

df <- rbind.data.frame(NP,ST,MT,LT)

bayes_plot_facets(df) +
  scale_fill_brewer(palette="PuBuGn")


BS_BY_H <- vector("list", 24)
for (i in 1:24) {
  BS_BY_H[[i]] <-
    lapply(L_h, function(x) {
      sapply(x, function(z) {unname(z[i])})
    })

  BS_BY_H[[i]] <-
    bayes_analysis(BS_BY_H[[i]],benchmark,
                 rope = 2.5)["AR+VEST", ]
}
BS_BY_H <- do.call(rbind, BS_BY_H)
bayes_plot2(BS_BY_H) +
  scale_fill_brewer(palette="PuBuGn")

