load("experiments/data/datasets.rdata")

library(vest)

max_sample_size <- 3000
ts_list = ts_list[sapply(ts_list, length) >= max_sample_size]

n <- length(ts_list)
IDS <- 1:42
sample_size <- seq(from = 100, to = max_sample_size, by = 100)
m5.fun <- list(M5.train, M5.predict)

FINALRESULTS <- vector("list", n)
for (i in IDS) {
  #i<-1
  cat(i, "\n")
  x <- ts_list[[i]]

  x <- head(x, max_sample_size)

  results_by_size <- vector("list", length(sample_size))
  for (j in 1:length(sample_size)) {
    cat(j,"/", length(sample_size),"\n")
    #j<-1
    SSIZE <- sample_size[j]

    xj <- head(x, SSIZE)
    freq <- frequency(x)

    train <- xj[ seq_len(.8 * SSIZE)]
    test <-  xj[-seq_len(.8 * SSIZE)]

    train <- ts(train, frequency = freq)
    test <- ts(test, frequency = freq)

    tres1 <- WF_part1(train = train, test = test, h = 2)

    tres2 <- WF_part2_direct2(tres1, learning_functions = m5.fun, h = 2)

    y_test <- tres1$data$test[,"target1"]
    y_train <- tres1$data$train[,"target1"]
    freq <- tres1$feature_model@keys$freq


    yhat_arima <-
      arima_multistep(
        train = y_train,
        test = y_test,
        freq = freq,
        h = 2
      )

    yhat_ets <-
      tryCatch(ets_multistep(
        train = y_train,
        test = y_test,
        freq = freq,
        h = 2
      ),
      error = function(e) yhat_arima)

    yhat_tbats <-
      tbats_multistep(
        train = y_train,
        test = y_test,
        freq = freq,
        h = 2
      )

    tres2$yhat <-
      c(tres2$yhat,
        ARIMA = list(yhat_arima),
        ETS = list(yhat_ets),
        TBATS = list(yhat_tbats))

    tres2$yhat <-
      lapply(tres2$yhat,
             function(x) x[,1])

    tres2$y <- tres2$y[,1]

    results_by_size[[j]] <- tres2
  }

  FINALRESULTS[[i]] <- results_by_size

  SIGNATURE <- paste0("F",IDS[1],"_",IDS[length(IDS)])
  save(FINALRESULTS,file = paste0("experiments/VEST_PT2_LC_TK33000_",SIGNATURE,".rdata"))
}

