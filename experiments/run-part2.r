load("./experiments/VEST_pt1_F1_2.rdata")

library(vest)
n <- length(RESULTS)
IDS <- 1:n

FINALRESULTS <- vector("list", n)
for (i in IDS) {
  cat(i, "\n")
  x <- RESULTS[[i]]

  m5.fun <- list(M5.train, M5.predict)
  #xgb.fun <- list(XGB.train, XGB.predict)

  tres <- WF_part2(x, learning_functions = m5.fun)

  FINALRESULTS[[i]] <- tres

  SIGNATURE <- paste0("F",IDS[1],"_",IDS[length(IDS)])
  save(FINALRESULTS,file = paste0("experiments/VEST_pt2_",SIGNATURE,".rdata"))
}

