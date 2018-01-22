##Not Near Finished!!
##Deep Recurrent Network to Forecast Next Period Covariance Matrix of a Selection of Stocks
##Eventual plugin to portfilio optimizers
##Carter Raha
library(alphavantager)
library(tidyquant)

APIKEY <- "api-key-here"

#Dow 30 - Just some tickers 
Stock_List <- c("MMM", "AXP", "AAPL", "CAT", "CVX", "CSCO", "KO", "DWDP", "DIS", "XOM", "GE", "GS", "HD", "IBM", "INTC", "JNJ",
                "JPM", "MCD", "NKE", "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "WMT")

#Get Stocks nicely! 
#Alpha Vantage is a free stock API. Lets not flood their servers with quick requests in succesion
for (i in 1:length(Stock_List)) {
  assign(Stock_List[i], av_get(symbol = Stock_List[i], av_fun = "TIME_SERIES_DAILY", outputsize='full'))
  Sys.sleep(3)
}

#Make Training Data
picks <- matrix(rep(c(0,0,0), 20), ncol = 3)


List_Stocks <- list(MMM, AXP, AAPL, CAT, CVX, CSCO, KO, DWDP, DIS, XOM, GE, GS, HD, IBM, INTC, JNJ,
                 JPM, MCD, NKE, PFE, PG, TRV, UTX, UNH, VZ, WMT)

#Fibbonacci Num
cov_estimation_period <- 89

#use Eval to parameterize this function in future
##Take random 3 stocks and get the time series of covariance matricies 
listCovList <- list()
pb <- txtProgressBar(min = 0, max = nrow(picks), style = 3)
for (i in 1:nrow(picks)) {
  covList <- list()
  picks[i,] <- sample(1:26, 3, replace=FALSE)
  data1 <- List_Stocks[[picks[i,1]]] %>% 
    tq_mutate(select=close, mutate_fun = dailyReturn)
  data2 <- List_Stocks[[picks[i,2]]] %>% 
    tq_mutate(select=close, mutate_fun = dailyReturn)
  data3 <- List_Stocks[[picks[i,3]]] %>% 
    tq_mutate(select=close, mutate_fun = dailyReturn)
  if (nrow(data1) == nrow(data2) & nrow(data2) == nrow(data3)) {
      rets <- cbind(data1$daily.returns, data2$daily.returns, data3$daily.returns)
  } else {
    #handle when some stocks don't match up in length
    #Looking at you, DowDupont
    short <- which.min(c(nrow(data1), nrow(data2), nrow(data3)))
    next
  }
  for (j in cov_estimation_period:(nrow(rets)-1)){
    covList[[(j-cov_estimation_period+1)]] <- cov(rets[(j-cov_estimation_period):(j+1),])
  }
  listCovList[[i]] <- covList
  setTxtProgressBar(pb, i)
}
close(pb)

##Convert time series into training data
##Not testing yet, just a POC at the moment
# Given a cov matrix
# A B C
# D E F
# G H I 
#
# Turn it into a vector 
# A E I B C F 
# And take the cube root (bigger decimals easier to estimate?) while maintaining the negative
#
# 
#
#
#Handy Cube Root Func found https://gepsoft.com/forum/index.php?p=/discussion/132/cubed-root-function-in-r-gep3rt/p1
gep3Rt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

in_data2 <- array(rep(0, 1000*89*6), c(1000, 89, 6))
out_data2 <- matrix(0,nrow=1000,ncol=6)
listnum <- 1

for (listnum in 1:length(listCovList)) {
  if (length(listCovList[[listnum]])){
    templist <- listCovList[[listnum]]
    for (i in 1:length(templist)) {
      diagsupper <- c(diagsupper,gep3Rt(c(diag(templist[[i]]),templist[[i]][upper.tri(templist[[i]])])))
    }
    tempMat <- matrix(diagsupper,ncol=6,byrow = TRUE)
    for (i in 89:(length(templist)-1)) {
      in_data2[(i-88),,] <- tempMat[(i-88):i,1:6]
      out_data2[(i-88),] <- tempMat[(i+1),1:6]
    }
  }
}

#Model Creation Here
#Take 89 1x6 Vecs
#Run a GRU through it 
#Reduce dimensionality in Fibbonacci steps 
#Get an output of a 1x6 vec
#
#
#
library(keras)
model <- keras_model_sequential()
model %>%
  layer_gru(units=55,
             input_shape=c(89,6), activation = 'tanh',
             return_sequences=FALSE) %>%
  layer_dense(34, activation = 'tanh') %>% 
  layer_dense(21, activation = 'tanh') %>% 
  layer_dense(13, activation = 'tanh') %>% 
  layer_dense(6, activation = 'tanh')

model %>% compile(
  loss = 'mean_absolute_percentage_error',
  optimizer = optimizer_adamax(),
  metrics = c('accuracy')
)

summary(model)

history <- model %>% fit(
  in_data2, out_data2, 
  epochs = 500, batch_size = 5, 
  validation_split = 0.2
)



