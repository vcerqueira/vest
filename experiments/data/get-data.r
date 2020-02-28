library(tsdl)
library(forecast)
library(xts)
library(tsensembler)

min_len <- 1000
tsdl_list <- tsdl[sapply(tsdl, length) > min_len]
tsdl_list <-
  tsdl_list[!sapply(tsdl_list,
                    function(x) {
                      any(is.na(x))
                    })]

tsdl_list[] <- lapply(tsdl_list, head, 3000)

is_univar <- sapply(tsdl_list, class) == "ts"

tsdl_list <- tsdl_list[is_univar]

ids <- which(sapply(tsdl_list, frequency) == 1)
#attributes(tsdl_list[[5]])
#x<-tsdl_list[[5]]

freqs <- sapply(tsdl_list[ids],
                get_frq_dummy)

for (i in 1:length(ids)) {
  ID <- ids[i]
  freq_i <- freqs[i]

  tsdl_list[[ID]] <- ts(tsdl_list[[ID]], frequency = freq_i)
}


get_frq_dummy <-
  function(x) {
    require(forecast)

    freq <- frequency(x)
    #x <- tail(x, 150)
    x <- head(x, 150)
    #x <- ts(x, frequency = freq)

    tests <- c("seas", "ocsb","hegy", "ch")
    freqx <- c(7, 12, 24, 30, 48, 60,168)

    get.freq <-
      sapply(freqx,
             function(fr) {
               x.fr <- ts(x, frequency = fr)
               #tryCatch(nsdiffs(x.fr), error=function(e) 0)

               sapply(tests,
                      function(z) {
                        tryCatch(nsdiffs(x.fr, test=z),
                                 error=function(e) 0)
                      })
             })

    any_flag <- colSums(get.freq)
    if (any(any_flag > 0)) {
      id <- which.max(any_flag)
      r <- freqx[id]
    } else {
      #id <- NA
      r <- NA
    }

    r
    # any_coef <-
    #   sapply(freqx,
    #          function(fr) {
    #            cat(".")
    #            x.fr <- ts(tail(x,150), frequency = fr)
    #
    #            m <- auto.arima(x.fr, stepwise = TRUE)
    #
    #            coef <- m$coef
    #            as.integer(any(grepl("^s", names(coef))))
    #          })
    #
    # r <- dummy.freq + any_coef

  }



load("data/tseries_vcerq.rdata")

ids2rm <- c(14:21,24,28,35:42,53:61)

ts_list <- ts_list[-ids2rm]

tdiff <-
  sapply(ts_list,
         function(x) {
           xt <-
             tryCatch(
               as.POSIXct(head(rownames(as.data.frame(x)))),
               error = function(e)
                 NA
             )

           difftime(xt[2],xt[1], units = "hours")
         })

#lapply(ts_list[is.na(tdiff)], head, 3)
tdiff[is.na(tdiff)] <- 1

FRQs <- tdiff
FRQs[tdiff == 1] <- 24
FRQs[tdiff == .5] <- 48
FRQs[tdiff == 24] <- 365

ts_list <-
  lapply(1:length(ts_list),
         function(i) {
           x <- ts_list[[i]]
           x <- ts(as.vector(x), frequency = FRQs[i])
           x <- head(x, 3000)
           x
         })

ts_list <- c(tsdl_list,ts_list)

#save(ts_list, file = "data/datasets.rdata")
#sapply(ts_list, frequency)
