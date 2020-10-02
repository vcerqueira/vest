load("./experiments/VEST_PT1_25092020.rdata")

n <- length(RESULTS)

IDS <- 1:90

library(vest)

#lfun <- list(M5.train, M5.predict)
lfun <- list(LASSO.train, LASSO.predict)

H <- 2

FINALRESULTS <- vector("list", n)
for (i in IDS) {
  #i<-1
  cat(i, "\n")
  x <- RESULTS[[i]]

  SERIES_RESULTS <- vector("list", length(x))
  for (j in 1:length(x)) {
    #j<-1
    y_test <- x[[j]]$data$test[,"target1"]
    y_train <- x[[j]]$data$train[,"target1"]
    freq <- x[[j]]$feature_model@keys$freq

    #j<-1
    tres <- WF_part2_direct2(x = x[[j]],
                     learning_functions = lfun,
                     h = H)

    yhat_cls <-
      run_classical_methods(y_train = y_train,
                            y_test = y_test,
                            freq = freq,
                            h = H)

    tres$yhat <- c(tres$yhat, yhat_cls)

    SERIES_RESULTS[[j]] <- tres
  }

  FINALRESULTS[[i]] <- SERIES_RESULTS

  SIGNATURE <- paste0("F", IDS[1], "_", IDS[length(IDS)])
  save(FINALRESULTS,
       file = paste0("experiments/VEST_PT2_DIR_LASSO_",
                     SIGNATURE, ".rdata"))
}
SIGNATURE <- paste0("F", IDS[1], "_", IDS[length(IDS)])
a <- ""
save(a, file = paste0("FINITO_",SIGNATURE,".rdata"))
