bp_grouped <-
  function(x, group_name) {
    ggplot(x,
           aes(x = variable, y = value, color=group_name)) +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=6,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            legend.position = "top")
  }

prep_pd_boxplot <-
  function(x) {
    x <- do.call(rbind, x)

    id <- 1
    x <- as.data.frame(x)
    pd <-
      lapply(x, function(z) {
        percentual_difference(z, x[, 1])
      })

    pd[id]<-NULL

    pd
  }

bp_dist <-
  function(ranks) {
    if (!is.data.frame(ranks)) {
      ranks <- as.data.frame(ranks)
    }

    avg.rank <- apply(ranks,2,median,na.rm=TRUE)
    nms.sort <- names(sort(avg.rank))

    ranks <- ranks[,nms.sort]
    ranks <- as.data.frame(ranks)

    x <- melt(ranks)

    p <- ggplot(x, aes(variable, value))

    p <- p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=6,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 10),
            axis.title.y = element_text(size = 10))

    p
  }

complete_list <-
  function(my_list, unames=NULL) {
    if (is.null(unames)) {
      ml <- unlist(unname(my_list))
      unames <- unique(names(ml))
    }

    for (i in 1:length(my_list)) {
      x <- my_list[[i]]
      miss_nm <- unames[!unames %in% names(x)]

      dm <- rep(NA_real_, times = length(miss_nm))
      names(dm) <- miss_nm
      x <- c(x, dm)
      my_list[[i]] <- x
    }

    my_list
  }

box_plot3 <-
  function(x,take_logs) {
    require(reshape2)
    require(ggplot2)

    xp <- melt(as.data.frame(x))

    if (take_logs) {
      xp$value <- log_trans(xp$value)
    }

    p <- ggplot(xp, aes(x = 1, y = value)) +
      facet_wrap(~ variable,
                 nrow = 1,
                 scales = "free_x") +
      geom_boxplot() +
      geom_hline(yintercept = 0, col = "red") +
      theme_minimal() +
      labs(x = "",
           y = "Log. Percentual Diff.") +
      theme(axis.text.x  = element_blank()) +
      theme(
        axis.text.y  = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(angle = 90, size = 12)
      )

    p
  }


bayes_analysis <-
  function(x, baseline, rope=2.5) {
    f <- BayesianSignTest

    x <- do.call(rbind, x)
    x <- as.data.frame(x)
    x <- x[complete.cases(x),]

    xpd <-
      lapply(x,
             function(z) {
               percentual_difference(z, x[,baseline])
             })

    BA <-
      sapply(xpd,
             function(z) {
               r <-
                 f(diffVector = z,
                   rope_min = -rope,
                   rope_max = rope)

               unlist(r)

             })

    BA <- t(BA)

    colnames(BA) <- c("Benchmark loses", "Draw", "Benchmark wins")

    BA
  }


bayes_plot_facets <-
  function(x) {
    require(reshape2)
    # x <- t(x)
    # x <- as.data.frame(x)
    # x$Result <- rn <- rownames(x)
    # rownames(x) <- NULL

    xDF <- melt(x)
    xDF$Result <- factor(xDF$Result,levels = unique(xDF$Result))
    xDF$Horizon <- factor(xDF$Horizon,levels = unique(xDF$Horizon))
    #head(xDF)

    colnames(xDF)[3]<-"Method" #<- c("Result", "Method", "value")

    ggplot(xDF, aes(x = Method,
                    y = value,
                    fill = Result)) +
      facet_wrap(~Horizon) +
      geom_col(position = "fill") +
      ylab("Proportion of probability") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x  = element_text(
        angle = 45,
        size = 10,
        hjust = 1
      ),
      legend.position = "top") +
      theme(axis.text.y  = element_text(size = 11),
            axis.title.y = element_text(size = 11))

  }

bayes_plot <-
  function(x) {
    require(reshape2)
    x <- t(x)
    x <- as.data.frame(x)
    x$Result <- rn <- rownames(x)
    rownames(x) <- NULL

    xDF <- melt(x)
    xDF$Result <-
      factor(xDF$Result,levels = rn)
    #head(xDF)

    colnames(xDF) <- c("Result", "Method", "value")

    ggplot(xDF, aes(x = Method,
                    y = value,
                    fill = Result)) +
      geom_col(position = "fill") +
      ylab("Proportion of probability") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x  = element_text(
        angle = 90,
        size = 10,
        hjust = 1
      ),
      legend.position = "top") +
      theme(axis.text.y  = element_text(size = 11),
            axis.title.y = element_text(size = 11))

  }

bind_and_avgrank <-
  function(x, ids=NULL) {
    x <- do.call(rbind, x)
    if (!is.null(ids)) {
      x <- x[,ids]
    }

    ranks_ <- apply(x, 1, rank)
    avgrank <- rowMeans(ranks_)
    sdevrank <- apply(ranks_,1,sd)

    list(avgrank=avgrank,sdevrank=sdevrank)
  }

#https://github.com/M4Competition/M4-methods
mase_cal <- function(insample, outsample, forecasts) {
  stopifnot(stats::is.ts(insample))
  #Used to estimate MASE
  frq <- stats::frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)

  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}

bayes_plot2 <-
  function(x) {
    require(reshape2)
    x <- t(x)
    x <- as.data.frame(x)
    x$Result <- rn <- rownames(x)
    rownames(x) <- NULL

    xDF <- melt(x)
    xDF$Result <- factor(xDF$Result, levels = rn)
    xDF$variable <- as.character(rep(1:24, each=3))
    xDF$variable <- factor(xDF$variable,
                           levels = sort(as.numeric(unique(xDF$variable))))

    colnames(xDF) <- c("Result", "Method", "value")

    ggplot(xDF, aes(x = Method,
                    y = value,
                    fill = Result)) +
      geom_col(position = "fill") +
      ylab("Proportion of probability") +
      xlab("Horizon") +
      theme_minimal() +
      theme(axis.text.x  = element_text(
        angle = 0,
        size = 10,
        hjust = 1
      ),
      legend.position = "top") +
      theme(axis.text.y  = element_text(size = 11),
            axis.title.y = element_text(size = 11))

  }

err_by_h_mase <-
  function(yhat, y, y_tr, frq) {
    insample <- ts(y_tr[,1], frequency = frq)

    forecastsNaiveSD <- rep(NA,frq)
    for (j in (frq+1):length(insample)){
      forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
    }

    masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)

    nextp <- 1
    short_term <- 1:8
    med_term <- 9:16
    long_term <- 17:24

    ASE <- abs(yhat-y) / masep

    L <- mean(ASE)

    avg_by_h <- colMeans(ASE)

    L_h <- avg_by_h
    names(L_h) <- as.character(seq_along(L_h))

    L_np <- mean(avg_by_h[nextp])
    L_st <- mean(avg_by_h[short_term])
    L_mt <- mean(avg_by_h[med_term])
    L_lt <- mean(avg_by_h[long_term])

    list(
      L = L,
      L_np = L_np,
      L_st = L_st,
      L_mt = L_mt,
      L_lt = L_lt,
      L_h = L_h
    )
  }

avg_rank_plot <-
  function(avg,sdev) {
    require(ggplot2)

    ord <- names(sort(avg))
    methods <- names(avg)

    ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
    ds$methods <- factor(ds$methods, levels = ord)


    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat="identity",
               fill="darkgrey") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 45,
                                        size = 11)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      geom_errorbar(aes(ymin = avg - sdev,
                        ymax = avg + sdev),
                    width = .5,
                    position = position_dodge(.9)) +
      labs(x="",
           y="Avg. Rank",
           title = "")
  }
