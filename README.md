# Qfin
Quant Finance - Carter Raha

# Sector DCC Garch Model
This folder has a R script that will generate a DCC Garch model from historical Sector ETF Data. This is a portion of the code that drove my final project in my Financial Econometrics class at the University of Alabama. The DCC Garch model was created by Robert Engle (see citation below). There were promising applying the model to these assets but I would like to improve it. I plan to update this by building out a neural stochastic volatility model and plug those predictions into portfolio optimization. Can share the results and other code from my final project upon request. 

F. Engle, Robert. (2002). Dynamic Conditional Correlation: A Simple Class of Multivariate Generalized Autoregressive Conditional Heteroskedasticity Models. Journal of Business & Economic Statistics. 20. 339-50. 

# Signals
This folder will hold any signals / strategies that I replicate from literature. It's likely I am also working on others that I don't share here. If you are interested in more of my signals and strategies, contact me to discuss.
In this folder are
* **ConnorsRSIPullback:** This is a mean reversion strategy that combines a short term RSI with other indicators to identify a pullback and ride the reversion to the mean.

# CovMatNet
This folder will hold a model I am creating. This model will use a deep recurrent network to forecast a covariance matrix for a selection of assets. This is a step ahead of forecasting the variance of an asset, as it not only captures the variance of a given stock, but how the stocks move relative to one another as well. This model is written in R and uses the Keras framework. 
