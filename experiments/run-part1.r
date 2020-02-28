load("experiments/data/datasets.rdata")

library(vest)
n <- length(ts_list)

IDS <- 1:2; n<-2# just 2 time series for running example
#IDS <- 1:90 # use the 90 time series to reproduce experiments

RESULTS <- vector("list", n)
for (i in IDS) {
  cat(i, "\n")
  x <- ts_list[[i]]

  ### #subsetting the time series for running example
  x <- head(x, 300)## dont do this when trying vest in your data
  ####

  tres <- WF_part1(x)

  RESULTS[[i]] <- tres

  SIGNATURE <- paste0("F",IDS[1],"_",IDS[length(IDS)])
  save(RESULTS, file = paste0("experiments/VEST_pt1_",SIGNATURE,".rdata"))
}

