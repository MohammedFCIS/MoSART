library(xts)
library(DMwR2)
library(quantmod)
library(TTR)
library(performanceEstimation) 
library(nnet)
library(e1071)
library(kernlab)
library(earth)
library(randomForest)
library(PerformanceAnalytics)

stock <- getSymbols("AAPL", auto.assign = FALSE)

# Define the Predictive Task
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean) # function HLC() extracts the High, Low, and Close quotes
  v[1] <- Cl(quotes)[1]           
  
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x)
  
  x <- apply(r,1,function(x) 
    sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN=avgPrice, col=1, legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind, col='red', legend='tgtRet')
candleChart(xts::last(stock,'3 months'), theme='white', 
            TA=c(
              addAvgPrice(on=1), 
              addT.ind()))

mySMA <- function(x) SMA(Cl(x))[,'SMA'] # Moving Average 
myATR <- function(x) ATR(HLC(x))[,'atr'] # Average True Range, measures volatility of series  
mySMI <- function(x) SMI(HLC(x))[, "SMI"] #  Stochastic Momentum Index 
myADX <- function(x) ADX(HLC(x))[,'ADX'] # Welles Wilder's Directional Movement Index 
myAroon <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator # Identify starting trends
myBB <- function(x) BBands(HLC(x))[, "pctB"] # Bollinger Bands
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1] # Chaikin Volatility
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1] # Close Location Value 
myEMV <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2] # Arms' Ease of Movement Value 
myMACD <- function(x) MACD(Cl(x))[,2] # Moving Average Convergence Divergence
myMFI <- function(x) MFI(HLC(x), Vo(x)) # Money Flow Index
mySAR <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1] # Parabolic Stop-and-Reverse
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1] # volatility

data.model <- specifyModel(T.ind(stock) ~ Delt(Cl(stock),k=1:10) + 
                             myATR(stock) + 
                             mySMI(stock) + 
                             myADX(stock) + 
                             myAroon(stock) + 
                             myBB(stock) + 
                             myChaikinVol(stock) + 
                             myCLV(stock) + 
                             CMO(Cl(stock)) + 
                             EMA(Delt(Cl(stock))) + 
                             myEMV(stock) + 
                             myVolat(stock) + 
                             myMACD(stock) + 
                             myMFI(stock) + 
                             RSI(Cl(stock)) + 
                             mySAR(stock) + 
                             runMean(Cl(stock)) + 
                             runSD(Cl(stock)))
set.seed(1234) 
rf <- buildModel(data.model,method='randomForest', 
                 training.per=c("1997-01-01","2017-12-30"), 
                 ntree=1000, 
                 importance=TRUE) 
varImpPlot(rf@fitted.model, type = 1) # Type 2 shows ranking based on decrease in node impurity 

imp <- importance(rf@fitted.model, type = 1) 
rownames(imp)[which(imp > 30)] 

# Regression
Tdata.train <- as.data.frame(modelData(data.model,
                                       data.window=c('2007-01-03','2017-01-03'))) # convert to data frame
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2017-01-04','2018-12-19')))) 
Tform <- as.formula('T.ind.stock ~ .') # the formula to be used in models

# Classification
buy.thr <- 0.1 
sell.thr <- -0.1 
Tdata.trainC <- cbind(Signal=trading.signals(Tdata.train[["T.ind.stock"]], 
                                             buy.thr,sell.thr), 
                      Tdata.train[,-1]) 
Tdata.evalC <- cbind(Signal=trading.signals(Tdata.eval[["T.ind.stock"]], 
                                            buy.thr,sell.thr), 
                     Tdata.eval[,-1]) 
TformC <- as.formula("Signal ~ .")

head(Tdata.train)

exp <- performanceEstimation( 
  PredTask(Tform, Tdata.train[1:2000,], 'SP500'), 
  c(Workflow('standardWF', wfID="standSVM", # fixed window
             learner='svm',learner.pars=list(cost=10,gamma=0.01)), 
    Workflow('timeseriesWF', wfID="slideSVM", # time series workflow
             type="slide", relearn.step=90, # sliding window, slide every 90 rows 
             learner='svm',learner.pars=list(cost=10,gamma=0.01)) 
  ), 
  EstimationTask(metrics="theil",  
                 method=MonteCarlo(nReps=10,szTrain=0.5,szTest=0.25)))
# the "theil" metric is typically used in time series tasks and the used baseline is
# the last observed value of the target variable. It is calculated as: 
# sum( (t_i - p_i)^2 ) / sum( (t_i - t_{i-1})^2 ), where t_{i-1} is
# the last observed value of the target variable
summary(exp)
plot(exp)

# Artificial Neural Networks
## Regression Task
set.seed(1234)
norm.data <- data.frame(T.ind.stock=Tdata.train[[1]],scale(Tdata.train[,-1]))
nn <- nnet(Tform, norm.data[1:1000, ], size = 5, decay = 0.01, 
           maxit = 1000, linout = TRUE, trace = FALSE)
preds <- predict(nn, norm.data[1001:2000, ])

sigs.nn <- trading.signals(preds,0.1,-0.1) 
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.stock"], 0.1, -0.1) 
sigs.PR(sigs.nn,true.sigs)

## Classification Task
set.seed(1234) 
norm.data <- data.frame(Signal=Tdata.trainC$Signal,scale(Tdata.trainC[,-1])) 
nn <- nnet(Signal ~ ., norm.data[1:1000, ], size = 10, decay = 0.01, 
           maxit = 1000, trace = FALSE)
preds <- predict(nn, norm.data[1001:2000, ], type = "class") 
sigs.PR(preds, norm.data[1001:2000, 1])

#-----------------------------------------------------
# Support Vector Machines
## Regression Task
set.seed(1234)
sv <- svm(Tform, Tdata.train[1:1000, ], gamma = 0.001, cost = 100) # default gamma is 1/ncol
s.preds <- predict(sv, Tdata.train[1001:2000, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1) 
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.stock"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)

## Classification Task
ksv <- ksvm(Signal ~ ., Tdata.trainC[1:1000, ], C = 10) 
ks.preds <- predict(ksv, Tdata.trainC[1001:2000, ]) 
sigs.PR(ks.preds, Tdata.trainC[1001:2000, 1])

#-----------------------------------------------------
# Multivariate Adaptive Regression Splines
## Regression Task
e <- earth(Tform, Tdata.train[1:1000, ]) 
e.preds <- predict(e, Tdata.train[1001:2000, ]) 
sigs.e <- trading.signals(e.preds, 0.1, -0.1) 
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.stock"], 0.1, -0.1) 
sigs.PR(sigs.e, true.sigs)
plot(e)

#----------------------------------------------------
# The Trading Set up
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}

policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b') {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's') {
    # this is the money already committed to buy stocks
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  orders
}
#-------------------------------------------------
# A Trading Simulator
start <- 1
len.tr <- 1000 # first 1000 for training models 
len.ts <- 500 # next 500 for testing them 
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1) 
# The market quotes during this "testing period" 
# This will be used by the simulator 
# Note: you need the training data created previously! 
# 1001
date <- rownames(Tdata.train[start+len.tr,]) 
# 1:500
market <- stock[paste(date,'/',sep='')][1:len.ts]
# 1:1000
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01) 
# 1001:1500
p <- predict(s,Tdata.train[ts,]) 
sig <- trading.signals(p,0.1,-0.1) # predictions to signals 
## now using the simulated trader 
t1 <- trading.simulator(market,sig, 'policy.1', # the policy function name
                        list(exp.prof=0.05,bet=0.2,hold.time=30)) 
t1
summary(t1)
tradingEvaluation(t1)
plot(t1,market,theme='white',name='SP500')

t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)
plot(t2,market,theme='white',name='SP500')

#----------------------------------------------------
# Report
equityWF <- as.xts(t1@trading$Equity)
rets <- Return.calculate(equityWF)
chart.CumReturns(rets, main="Cumulative returns of the strategy", ylab="returns")
yearlyReturn(equityWF)
plot(100*yearlyReturn(equityWF), 
           main='Yearly percentage returns of the trading system')
table.DownsideRisk(rets)
plot(rets)
