load("experiments/data/datasets.rdata")

library(vest)
n <- length(ts_list)

IDS <- 1:90

unloadNamespace("vest")
library(vest)
nreps <- 10
trs <- 0.7
tss <- 0.1
H <- 2

RESULTS <- vector("list", n)
for (i in IDS) {
  cat(i, "\n")
  #i<-1
  x <- ts_list[[i]]
  #x <- head(x,300)
  freq <- frequency(x)

  n <- length(x)
  ORIGINS <- rep_holdout_origins(n, nreps, trs, tss)
  TRs <- ceiling(trs * n)
  TSs <- ceiling(tss * n)

  MCresults <- vector("list", nreps)
  for (j in 1:length(ORIGINS)) {
    #j<-1
    o <- ORIGINS[j]

    TRAIN <- x[(o - TRs):(o - 1)]
    TEST <- x[o:(o + TSs - 1)]

    TRAIN <- ts(TRAIN, frequency = freq)
    TEST <- ts(TEST, frequency = freq)

    results_ji <- WF_part1(train = TRAIN,
                           test = TEST,
                           h = H)

    MCresults[[j]] <- results_ji
  }

  RESULTS[[i]] <- MCresults

  SIGNATURE <- paste0("F", IDS[1], "_", IDS[length(IDS)])
  save(RESULTS,
       file =
         paste0("experiments/VEST_PT1_30092020_",
                SIGNATURE, ".rdata"))
}



