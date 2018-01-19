#Written by Carter Raha
#
#Here is a DCC Garch Model Implementation
#DCC Stands for Dynamic Conditional Correlation
#For more info on model: http://pages.stern.nyu.edu/~rengle/dccfinal.pdf
#Currently set up to model SPDR Sectors, plot predicted variances vs actuals
# and make a portfolio that optimizes to minimum variance. 
#GMV portfolio allows shorting


library(quantmod)
require(ccgarch)

symbolsToGet <- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY", "SPY")
# can add FROM argument to this to increase data. 
getSymbols(symbolsToGet)

sectors <- list(XLB, XLE, XLF, XLI, XLK, XLP, XLU, XLV, XLY)

today <- Sys.Date()
todayDay <- strsplit(as.character(today), "-")[[1]][3]
todayMonth <- strsplit(as.character(today), "-")[[1]][2]
todayYear <- strsplit(as.character(today), "-")[[1]][1]

#Daily into monthly
#lapply(sectors, function(x) x <- subset(x, (sapply(strsplit(row.names(x),"-"), FUN = '[',3) == todayDay)))
#sapply(sectors, function(x) x <- periodReturn(x, period = 'monthly', leading = FALSE))

Returns <- NULL
dayReturns <- NULL

#Daily into monthly
#lapply(sectors, function(x) x <- subset(x, (sapply(strsplit(row.names(x),"-"), FUN = '[',3) == todayDay)))
listReturns <-  lapply(seq_along(sectors), function(x) Returns$x <- periodReturn(sectors[[x]], period = 'monthly', leading = FALSE))

Returns <- as.data.frame(listReturns)
colnames(Returns) <- symbolsToGet[1:9]

if (is.na(Returns[1,])) {Returns <- Returns[-1,]}

EstimationPeriod <- 60
TotalObvs <- dim(Returns)[1]
N.assets <- dim(Returns)[2]
ForecastPeriod <- TotalObvs-EstimationPeriod+1
one.vector <- matrix(1, N.assets, 1)
sigma <- list()
forecastH <- portfolio.weights <- dayWeights <- selectionMean<- matrix(0,ForecastPeriod, N.assets,dimnames = list(c(),symbolsToGet[1:9]))

for (i in 1:ForecastPeriod) {
  selection <- Returns[i:(EstimationPeriod+i-1),]
  selectionMeans <- colMeans(selection)
  selectionMean[i,] <- selectionMeans
  selectionDm <- selection - matrix(colMeans(selection), EstimationPeriod , N.assets, byrow = T)
  
  a.step <- diag(cov(Returns))
  A.step <- diag(rep(.3,N.assets))
  B.step <- diag(rep(0.75,N.assets))
  
  DCC.parameters.step <- c(0.01,0.98)
  
  DCC.regression.step <- dcc.estimation(inia = a.step, iniA = A.step, iniB = B.step, ini.dcc = DCC.parameters.step, dvar = selection, model = "diagonal")
  
  Var.step <- DCC.regression.step$h
  Coeff.step <- DCC.regression.step$out[1,]
  a.step <- matrix(Coeff.step[1:N.assets],N.assets,1)
  A.step <- diag(Coeff.step[(N.assets+1):(2*N.assets)])
  B.step <- diag(Coeff.step[(2*N.assets+1):(3*N.assets)])
  phi.step <- Coeff.step[(3*N.assets+1):(3*N.assets+2)]
  
  forecastH[i,] <- a.step+A.step%*%(as.numeric(selection[EstimationPeriod,]^2))+B.step%*%t(t(Var.step[EstimationPeriod,]))
  DCC.DCC.step <- matrix(DCC.regression.step$DCC[EstimationPeriod,],N.assets, N.assets)
  tildeE <- matrix(as.numeric(selection[EstimationPeriod,]/sqrt(Var.step[EstimationPeriod,])),N.assets,1)
  barR <- cor(selection)
  Q.step <- (1-phi.step[1]-phi.step[2])*barR+phi.step[1]*(tildeE%*%t(tildeE))+phi.step[2]*DCC.DCC.step
  #R t+1
  RR <- solve(diag(sqrt(diag(Q.step))))%*%Q.step%*%solve(diag(sqrt(diag(Q.step))))
  sigma[[i]] <- diag(sqrt(forecastH[i,]))%*%RR%*%diag(sqrt(forecastH[i,]))
  portfolio.weights[i,] <- (solve(sigma[[i]])%*%one.vector)/as.numeric(t(one.vector)%*%solve(sigma[[i]])%*%one.vector)
}

plotVars <- function(Symbol, ForecastPeriod, EstimationPeriod, sigma) {
  vari <- NULL
  forecasted <- NULL
  indexsym <- which(Symbol == symbolsToGet)
  for (i in 1:ForecastPeriod) {
    vari <- rbind(vari, var(Returns[(i+1):(EstimationPeriod+i),indexsym]))
  }
  vari <- na.omit(vari)
  row.names(vari) <- row.names(Returns)[(EstimationPeriod+1):nrow(Returns)]
  for (i in 1:(ForecastPeriod-1)) {
    forecasted <- rbind(forecasted, sigma[[i]][indexsym,indexsym])
  }
  dates <- seq(as.Date(row.names(vari)[1], format = "%Y-%m-%d"),
               by = "months", length = length(vari))
  plot(dates, sqrt(vari), type = 'l')
  lines(dates, sqrt(forecasted), type = 'l', col = 'red')
  title(Symbol)
  
}

par(mfrow = c(3,3))
plotVars("XLB", ForecastPeriod, EstimationPeriod, sigma)
legend(x="topright", lwd = 2, legend = c("Actual", "Forecasted"), col = c("black", "red"), lty = "solid")
plotVars("XLE", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLF", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLI", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLK", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLP", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLU", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLV", ForecastPeriod, EstimationPeriod, sigma)
plotVars("XLY", ForecastPeriod, EstimationPeriod, sigma)

portfolio <- NULL
portfolio.long <- NULL
portfolio.weights.Long <- NULL

portfolio.weights.Long <- portfolio.weights*(portfolio.weights>0)
for (i in 1:ForecastPeriod-1) {
  y1 <- Returns[EstimationPeriod+i,]
  portfolio[i] <- portfolio.weights[i,]%*%t(y1)
  portfolio.long[i] <- portfolio.weights.Long[i,]%*%t(y1)
}


spyRet <- periodReturn(Cl(SPY), period = 'monthly', leading = FALSE)
spyRet2 <- spyRet[(EstimationPeriod+2):length(spyRet)]
dates <- seq(as.Date(row.names(Returns)[(EstimationPeriod+1)], format = "%Y-%m-%d"),
             by = "months", length = length(spyRet2))
par(mfrow = c(1,1))
plot(dates, cumsum(portfolio.long), type = 'l', ylab = "Cum Sum Returns")
lines(dates, cumsum(portfolio), type = 'l', col = 'blue')
lines(dates, cumsum(spyRet2), type = 'l', col = 'red')
title("Global Minimum Variance vs SPY")
legend(x="topleft", lwd = 2, legend = c("GMV Long Only", "GMV LongShort", "SPY"), col = c("black", "blue", "red"), lty = "solid")
