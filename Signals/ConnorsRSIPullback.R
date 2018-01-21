#I am getting data from alpha vantage in this and using the very handy tidyquant package
#Learn more about alpha vantage here: https://www.alphavantage.co/
#This strategy is called the ConnorsRSI Pullback Strategy
library(alphavantager)
library(tidyquant)

APIKEY <- 'your-api-key'

# Set API Key (Do this once)
av_api_key(APIKEY)

# Get Time Series Data
MSFT_raw <- av_get(symbol = "MSFT", av_fun = "TIME_SERIES_DAILY", outputsize='full')
FB_raw <- av_get(symbol = "FB", av_fun = "TIME_SERIES_DAILY", outputsize='full')
DIS_raw <- av_get(symbol = "DIS", av_fun = "TIME_SERIES_DAILY", outputsize='full')

#StreakFunc
get_streak <- function(increase_or_decrease) {
  streak <- 0
  streak <- c(streak, increase_or_decrease[2])
  for (i in 3:length(increase_or_decrease)) {
    if (sign(increase_or_decrease[i-1]) == sign(increase_or_decrease[i])) {
      streak <- c(streak, (streak[length(streak)] + increase_or_decrease[i]))
    } else if (sign(increase_or_decrease[i-1]) != sign(increase_or_decrease[i])) {
      streak <- c(streak, (increase_or_decrease[i]))
    } 
  }
  return(streak)
}
#get days long Func
get_days_long <- function(enters, exits) {
  longs <- NULL
  for (i in 1:length(enters)) {
    if (!is.na(enters[i])){
      if ((enters[i] == 1 | longs[length(longs)] == 1) & exits[i] != 1 ) {
        longs <- c(longs, 1)
      } else {
        longs <- c(longs, 0)
      }
    } else {
      longs <- c(longs, 0)
    }
  }
  return(longs)
}

#Price_Below_Close_Threshold  #2,4,6,8 %
#Price_In_Bottom_X_Range  #10 or 25%
#ConnorsRSI_Threshold  #5 to 15
#Buy_On_Limit_Threshold  #4,5,8,10%
#ConnorsRSI_Exit #50,60,70,80

#Function takes in OHLC +Volume daily data and outputs a large dataframe with "Days Long" being the days you are long in a trade. 
#TODO: Plot / display results 
#       Investigate the low number of trades, testing with FB and MSFT, those stocks might be not be suited for this, need to debug
ConnorsRSIFunc <- function(Stock, Price_Below_Close_Threshold = .02, Price_In_Bottom_X_Range = .25,
                           ConnorsRSI_Threshold = 15, Buy_On_Limit_Threshold = .06, ConnorsRSI_Exit = 70) {
  
  Out <- Stock %>% 
            tq_mutate(select=close, mutate_fun = dailyReturn) %>%
            tq_mutate(select=volume, mutate_fun = SMA, n = 21, col_rename='Vol21') %>%
            mutate(range=high - low) %>%
            tq_mutate(select=range, mutate_fun = SMA, n = 20) %>%
            mutate(VOL20 = (daily.returns / SMA)) %>% 
            tq_mutate(select=close, mutate_fun = RSI, n=3, col_rename="RSI") %>%
            mutate(increase_decrease=ifelse(close > lag(close), 1, ifelse(close < lag(close), -1, 0))) %>%
            mutate(streak=get_streak(increase_decrease)) %>%
            tq_mutate(select=streak, mutate_fun = RSI, n=2,col_rename = "RSI_STREAK_2") %>%
            tq_mutate(select=daily.returns, mutate_fun= runPercentRank, n=100, col_rename = "PercentRank_100") %>%
            mutate(ConnorsRSI_3_2_100=(RSI+RSI_STREAK_2+PercentRank_100)) %>%
            tq_mutate(select=c(high,low,close),mutate_fun=ADX) %>%
            mutate(low_below_last_close=(low/lag(close))) %>%
            mutate(closing_percentile=((close-low)/(high-low))) %>%
            mutate(buy_at_price=(lag(close)*(1-Buy_On_Limit_Threshold))) %>%
            mutate(enter_signal=ifelse(
              lag(close) >= 5 &
                lag(Vol21) >= 250000 &
                lag(ADX) >= 30 &
                lag(low_below_last_close) <= (1-Price_Below_Close_Threshold) &
                lag(closing_percentile) <= Price_In_Bottom_X_Range &
                lag(ConnorsRSI_3_2_100) <= ConnorsRSI_Threshold
              , 1, 0)) %>%
            mutate(exit_signal=ifelse(
              ConnorsRSI_3_2_100 >= ConnorsRSI_Exit
              ,1,0)) %>%
            mutate(Days_Long=get_days_long(enter_signal, exit_signal))
            
  return(Out)

}

outData <- lapply(list(MSFT_raw, FB_raw, DIS_raw),ConnorsRSIFunc)
outData
