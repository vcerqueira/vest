load("/Users/vcerqueira/Desktop/VEST_PT2_LC_TK33000_ALL.rdata")

source("experiments/analysis-src.r")

library(hrbrthemes)
library(GGally)
library(viridis)

fresults <- vector("list", length(FINALRESULTS))
for (i in 1:length(FINALRESULTS)) {
  #i<-2
  cat(i, "\n")
  x <- FINALRESULTS[[i]]

  LCresults <- vector("list", length(x))
  for (j in 1:length(x)) {
    #j<-1
    #cat("j ", j, "\n")
    iter_results <- x[[j]]

    #names(iter_results$yhat)

    iter_results$yhat <-
      iter_results$yhat[c(1,4,6,7,8)]


    names(iter_results$yhat) <-
      c( "AR", "AR+VEST","ARIMA","ETS","TBATS")


    frq <- iter_results$feats_model@keys$freq
    if (frq > NROW(iter_results$y)) {
      frq <- 1
    }

    err <-
      sapply(iter_results$yhat,
             function(z) {
               #cat("-")
               err_by_h_mase(
                 yhat = z,
                 y = as.matrix(iter_results$y),
                 y_tr = iter_results$y_tr,
                 frq = frq
               )$L_np
             })

    LCresults[[j]] <- err
  }

  LCresults <- do.call(rbind, LCresults)

  fresults[[i]] <- LCresults
}

colnames(fresults[[1]])
ranksOneStep <- #fresults
  lapply(fresults,
         function(x) {
           t(apply(x,1,rank))
         })

library(tsensembler)
avgRankOS <- apply(simplify2array(ranksOneStep), 1:2, mean, na.rm=TRUE)
avgRankOS <- as.data.frame(avgRankOS)
avgRankOS_Sm <- roll_mean_matrix(avgRankOS, 10)
#plot_learning_curve(avgRankOS_Sm)

avg <- avgRankOS_Sm

avg <- as.data.frame(t(avg))

avg$Method <- rownames(avg)
rownames(avg) <- NULL
colnames(avg)[1:30] <- as.character(seq(from=100,to=3000, by=100))
data<-avg

#std,robust,uniminmax,globalminmax,center,centerObs

ggparcoord(data,
           columns = 1:30,
           groupColumn = 31, #order = "anyClass",
           showPoints = TRUE,
           scale = "globalminmax",
           #title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
) +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=10),
    legend.position = "top", axis.text.x = element_text(angle=30)
  ) +
  xlab("Sample Size") +
  ylab("Average Rank")

