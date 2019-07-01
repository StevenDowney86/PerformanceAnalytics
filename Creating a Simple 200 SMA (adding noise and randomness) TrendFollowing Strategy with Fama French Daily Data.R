library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(boot)

head(FF_3_Factor_and_Momentum)

set.seed(100)
#add noise of 10% annual vol
noise <- rnorm(nrow(FF_3_Factor_and_Momentum$MKT_Plus_Rf),
      mean = 0, sd = .10/(252^.5))
#add noise of 5% annual vol
noise2 <- rnorm(nrow(FF_3_Factor_and_Momentum$MKT_Plus_Rf),
               mean = 0, sd = .05/(252^.5))
#without noise
FamaFrench1 <- 1 + (cumprod(1 + (FF_3_Factor_and_Momentum$MKT_Plus_Rf)) - 1)
tail(FamaFrench1, n = 50)

#added noise to see how robust the signal is for slight variation
FamaFrench <- 1 + (cumprod(1 + (FF_3_Factor_and_Momentum$MKT_Plus_Rf+noise)) - 1)
tail(FamaFrench, n = 50)

FamaFrench2 <- 1 + (cumprod(1 + (FF_3_Factor_and_Momentum$MKT_Plus_Rf+noise2)) - 1)
tail(FamaFrench1, n = 50)

different_paths <- merge(FamaFrench,FamaFrench1,FamaFrench2) %>% Return.calculate()
charts.PerformanceSummary(different_paths["1990/2012"])

#Testing a simple moving average strategy

#download data if needed
setDefaults(getSymbols.av, api.key = "YG136Y1AT346MEHI")
getSymbols("MSFT", src = "av", 
           output.size = "full", adjusted = TRUE)


#plot chart series
chartSeries(FamaFrench, TA=NULL)

#store data as an xts object
data <- FamaFrench

#create your Simple Moving Average
sma.200 <- SMA(na.locf(FamaFrench), 200)

#add the price data
sma.200$price <- data

#chart it so you get a feel
chartSeries(data["1929/1940"], TA='addSMA(200)')

#create the signal for SMA 200 strategy
#this signal says if the price is above the SMA buy, if not hold cash as 0
signal <- ifelse(sma.200$SMA < sma.200$price, 1, 0)

sma.200["1938-06/1938"]

#add slippage and transaction costs in percentage term including slippage and commissions per Philisophical Economics on historical costs
slippage_costs <- -.0060

#add the signal and the trend strategy returns
sma.200$signal <- signal

#add slippage and commision costs signal
sma.200$slippage_signal <- lag(ifelse(sma.200$signal != lag(sma.200$signal, 1), slippage_costs, 0), k = -1)


#calculate the returns based on the price data times the signals. must add to signal a day to buy on the day
#after the signal
sma.200$portfolio.return_no_costs <- (ROC(sma.200$price)*lag(signal))
sma.200$portfolio.return_withcosts <- sma.200$portfolio.return_no_costs + sma.200$slippage_signal

#merge the trendfollowing strategy and the buy and hold
Trendfollowing_vs_buyandhold <- na.omit(merge(
 FF_3_Factor_and_Momentum$MKT_Plus_Rf, sma.200[,c(5,6)]))

#chart the performance of buy and hold vs. trendfollowing
charts.PerformanceSummary(Trendfollowing_vs_buyandhold["2007/2010"])
charts.PerformanceSummary(FF_3_Factor_and_Momentum$MKT_Plus_Rf["1937/1940"])
charts.PerformanceSummary(FF_3_Factor_and_Momentum$MKT_Plus_Rf)
 table.AnnualizedReturns(Trendfollowing_vs_buyandhold["1999/2010"])                        
#calculate annualized returns
table.AnnualizedReturns(Trendfollowing_vs_buyandhold)

#calculate drawdown ratios
table.DrawdownsRatio(Trendfollowing_vs_buyandhold)
CalmarRatio(Trendfollowing_vs_buyandhold)
chart.Drawdown(Trendfollowing_vs_buyandhold)
top2 <- table.Drawdowns(Trendfollowing_vs_buyandhold$portfolio.return, top = 30)
top <- table.Drawdowns(Trendfollowing_vs_buyandhold$MKT_Plus_Rf, top = 30)

mean(top$Depth)
mean(top2$Depth)

chart.CumReturns(Trendfollowing_vs_buyandhold, ylog = TRUE)
?chart.CumReturns

sma.200[is.na(sma.200)] <- 0
head(sma.200, n = 250)

#calculate rolling outperformance
Trendfollowing_vs_buyandhold$Outperformance <- Trendfollowing_vs_buyandhold[,2] - Trendfollowing_vs_buyandhold[,1]
Trendfollowing_vs_buyandhold$RollingOutPerformance1year <- rollapply(Trendfollowing_vs_buyandhold[,3],
                                                                     width = 252,
                                                                     FUN = Return.annualized)

Trendfollowing_vs_buyandhold$RollingOutPerformance5year <- rollapply(Trendfollowing_vs_buyandhold[,3],
                                                                width = 252*5,
                                                                FUN = Return.annualized)

Trendfollowing_vs_buyandhold$Rolling5yearSharpeMarket <- rollapply(Trendfollowing_vs_buyandhold[,1],
                                                             width = 252*5,
                                                             FUN = SharpeRatio.annualized)



####not working yet for ggplot
sma.200$cumpricegrowth <- cumprod(100 + (FF_3_Factor_and_Momentum$MKT_Plus_Rf)) - 1
sma.200$cumpricegrowth_portfolio <- cumprod(100 + (sma.200$portfolio.return)) -1


library(ggplot2)
library(ggthemes)
library(extrafont)
#from http://joeystanley.com/blog/custom-themes-in-ggplot2
theme_joey <- function () { 
  theme_bw(base_size=12, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

#create ggplot
ggplotprices <- na.omit(merge(sma.200$cumpricegrowth,sma.200$cumpricegrowth_portfolio))

autoplot(ggplotprices, facets = NULL) +
  scale_y_continuous(trans = "log10") +
  theme_joey()
