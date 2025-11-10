library("viridis")
library("igraph")
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr) 
library("PerformanceAnalytics")
library("MALDIquant")
library("gridExtra"
library("caret")
library("hydraulics")
library("CHNOSZ")
library("gglm")
library("rpart")
library(stringr)
library("bnlearn")
library("rpart.plot")
library("liver")
library("e1071")
library("ggplot2")         # Graphics engine
library("RColorBrewer")    # Nice color palettes
library("plot3D")          # for 3d surfaces. 
library("dplyr")           # Better data manipulations
library("tidyr")           # gather variables into long format
library("parallel")        # mclapply for multicore processing
library("randomForestSRC") # random forests for survival, regression and 
library("OneR")
library("stringr") 
library("ggpubr")
library("bnlearn")
library("ggRandomForests") # ggplot2 random forest figures (This!)
library(readr)
library("randomForestExplainer")
library("neuralnet")
library("pheatmap")
library("sdcMicro")
library("summarytools")
library("ggpmisc")
library(ggfortify)
library("pheatmap")
library("ggRandomForests")
library("forecast")
library("sdcMicro")
library("pmhtutorial")
library("marima")
library(randomForest)
library(xts)
library(ggplot2)
library(tseries)
library(pheatmap)

library(feather) # data import
library(data.table) # data handle
library(rpart) # decision tree method
library(party) # decision tree method
library(forecast) # forecasting methods
library(randomForest) # ensemble learning method
library(ggplot2) # visualizations
library(MTS)
library(ggpubr)
library(tseries)
library("ie2misc")
library("Metrics")
library(tseries)
library("eseis")
library(ggplot2)
library(ggnewscale)
library("zoo")
library("rattle")
# Function for LjungBox
LjungBox<-function (x, lag = 1, type = c("Ljung-Box"), fitdf = 0) 
{
    if (NCOL(x) > 1) 
        stop("x is not a vector or univariate time series")
    DNAME <- deparse1(substitute(x))
    lag <- as.integer(lag)
    type <- match.arg(type)
    cor <- acf(x, lag.max = lag, plot = FALSE, na.action = na.pass)
    n <- sum(!is.na(x))
    PARAMETER <- c(df = lag - fitdf)
    obs <- cor$acf[2:(lag + 1)]
    if (type == "Box-Pierce") {
        METHOD <- "Box-Pierce test"
        STATISTIC <- n * sum(obs^2)
        PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
    }
    else {
        METHOD <- "Box-Ljung test"
        STATISTIC <- n * (n + 2) * sum(1/seq.int(n - 1, n - lag) * 
            obs^2)
        PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
    }
    names(STATISTIC) <- "X-squared"
    structure(list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME), 
        class = "htest")
}
