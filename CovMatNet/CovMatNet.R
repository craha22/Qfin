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
# Note D G H = B C F
# And take the cube root (bigger decimals easier to estimate?) while maintaining the negative
#
# 
#
#
#Handy Cube Root Func found https://gepsoft.com/forum/index.php?p=/discussion/132/cubed-root-function-in-r-gep3rt/p1
gep3Rt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

in_data4 <- array(rep(0, (10*4453)*89*6), c((10*4453), 89, 6))
out_data4 <- matrix(0,nrow=(10*4453),ncol=6)
listnum <- 1

for (listnum in 1:length(listCovList)) {
  if (length(listCovList[[listnum]])){
    templist <- listCovList[[listnum]]
    for (i in 1:length(templist)) {
      diagsupper <- c(diagsupper,gep3Rt(c(diag(templist[[i]]),templist[[i]][upper.tri(templist[[i]])])))
    }
    tempMat <- matrix(diagsupper,ncol=6,byrow = TRUE)
    for (i in 89:(length(templist)-1)) {
      in_data4[(i-88),,] <- tempMat[(i-88):i,1:6]
      out_data4[(i-88),] <- tempMat[(i+1),1:6]
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
             input_shape=c(89,6), activation = 'tanh',return_sequences=TRUE) %>%
  layer_gru(units=34, activation = 'tanh',
            return_sequences=FALSE) %>%
  layer_dense(21, activation = 'tanh') %>% 
  layer_dense(13, activation = 'tanh') %>% 
  layer_dense(8, activation = 'tanh') %>% 
  layer_dense(6, activation = 'tanh')

model %>% compile(
  loss = 'mse',
  optimizer = optimizer_adamax(),
  metrics = c('accuracy', 'mean_absolute_error')
)

summary(model)

history <- model %>% fit(
  in_data4, out_data4, 
  epochs = 150, batch_size = 55, 
  validation_split = 0.05
)

#Predict and check!
sum(cos(predict(model,expand_dims(in_data4[50,,]))-t(out_data4[50,])))
predict(model,expand_dims(in_data4[50,,]))
t(out_data4[50,])

#Establishing test data here
#I am recycling now to develop an end to end pipline
#Will test on holdout group soon when I've developed more
test_in_data <- in_data2[1:1000,,]
test_out_data <- out_data2[1:1000,]

#predict on row
pred_row <- function(x) {
  results <- predict(model,expand_dims(x))
  return(results)
}

#call prediciton on rows
model_preds <- apply(test_in_data,1,FUN=pred_row)

#I wonder what we can infer from changing covariances in the market (ie all clustered?)
model_preds <- t(model_preds)

testPredRow <- model_preds[1,]

#Reference:
# Given a cov matrix
# A B C  
# D E F
# G H I 
#
# Turn it into a vector 
# A E I B C F 
# 1 2 3 4 5 6 
# Note D G H = B C F
#Undo the data transforms to get back to the original covariance matrix
covmat_from_preds <- function(predRow) {
  predRow <- predRow ** 3 
  matrixVec <- c(predRow[1],predRow[4], predRow[5], 
                 predRow[4], predRow[2], predRow[6], 
                 predRow[5], predRow[6], predRow[3])
  return(matrixVec)
}

forcastedSigmas <- t(apply(model_preds, 1, covmat_from_preds))

plot(forcastedSigmas[,9], type = 'l')
vari = NULL
for (i in 1:length(listCovList[[1]])) {
  vari <- c(vari, listCovList[[1]][[i]][3,3])
}
plot(vari[90:1000], type = 'l')
lines(forcastedSigmas[,9], type = 'l', col = 'red')


##Generate Portfolio
#CVX, CSCO, CAT
CAT2 <- CAT %>% tq_mutate(select=close, mutate_fun = dailyReturn)
CSCO2 <- CSCO %>% tq_mutate(select=close, mutate_fun = dailyReturn)
CVX2 <- CVX %>% tq_mutate(select=close, mutate_fun = dailyReturn)

Returns <- cbind(CAT2$daily.returns,CSCO2$daily.returns, CVX2$daily.returns)
head(Returns)

CovMats <- NULL
#First Date is 2000-05-09 and back (index 89)
for (i in 89:nrow(Returns)) {
  CovMats[[(i-88)]] <- cov(Returns[(i-88):i,])
}


length(CovMats)

in_data3 <- array(rep(0, 5000*89*6), c(5000, 89, 6))
out_data3 <- matrix(0,nrow=5000,ncol=6)
diagsupper <- NULL
for (i in 1:length(CovMats)) {
  diagsupper <- c(diagsupper,gep3Rt(c(diag(CovMats[[i]]),CovMats[[i]][upper.tri(CovMats[[i]])])))
}
tempMat <- matrix(diagsupper,ncol=6,byrow = TRUE)
for (i in 89:(length(CovMats)-1)) {
  in_data3[(i-88),,] <- tempMat[(i-88):i,1:6]
  out_data3[(i-88),] <- tempMat[(i+1),1:6]
}
#First Date Predicted is 177, 2000-09-13

model_preds2 <- apply(in_data3,1,FUN=pred_row)
model_preds2 <- t(model_preds2) ##4364
forecastedSigmas2 <- t(apply(model_preds2, 1, covmat_from_preds))

one.vector <- matrix(1, 3, 1)
portfolio.weights <- matrix(0,ncol=3,nrow=5000)
portfolio <- NULL
eq_port <- NULL
for (i in 1:(nrow(forecastedSigmas2)-1)) {
  NextCovMat <- matrix(forecastedSigmas2[i,],ncol=3, byrow = TRUE)
  portfolio.weights[i,] <- t(solve(NextCovMat)%*%one.vector)/as.numeric(t(one.vector)%*%solve(NextCovMat)%*%one.vector)
  eq_weights <- c(.333,.333,.333)
  portfolio <- c(portfolio,portfolio.weights[i,]%*%Returns[(176+i),])
  eq_port <- c(eq_port,eq_weights%*%Returns[(176+i),])
}

plot(cumsum(portfolio), type = 'l')
lines(cumsum(eq_port), type = 'l', col='orange')
lines(cumsum(CVX2$daily.returns[177:4541]),col='red')
lines(cumsum(CSCO2$daily.returns[177:4541]),col='blue')
lines(cumsum(CAT2$daily.returns[177:4541]),col='green')
head(portfolio.weights)

var(eq_port)
var(portfolio)
var(CAT2$daily.returns)
var(CVX2$daily.returns)
var(CSCO2$daily.returns)

vari = NULL
for (i in 1:length(CovMats)) {
  vari <- c(vari, CovMats[[i]][1,1])
}
plot(vari[90:1000], type = 'l',ylim = c(0,.001), title='Forecasted vs Actual Variance: CAT')
legend(x="topright", lwd = 2, legend = c("Actual", "Forecasted"), col = c("black", "red"), lty = "solid")
lines(forecastedSigmas2[,1], type = 'l', col = 'red')

