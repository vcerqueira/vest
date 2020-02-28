load("experiments/VEST_pt2_F1_2.rdata")

n <- length(FINALRESULTS)
IDS <- 1:n

ARIMARESULTS <- vector("list", n)
for (i in IDS) {
  cat(i, "\n")
  x <- FINALRESULTS[[i]]

  y_test <- x$y[,"target1"]
  y_train <- x$y_tr[,"target1"]
  freq <- x$feats_model@keys$freq

  cn <- colnames(x$yhat$TR_VANILLA_ST)

  yhat_arima <-
    arima_multistep(
      train = y_train,
      test = y_test,
      freq = freq,
      h = 24
    )
  colnames(yhat_arima) <- cn

  ARIMARESULTS[[i]] <- yhat_arima

  SIGNATURE <- paste0("F",IDS[1],"_",IDS[length(IDS)])
  save(ARIMARESULTS,file = paste0("experiments/WF_ARIMA_",SIGNATURE,".rdata"))
}


