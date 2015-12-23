library(quantmod)
getSymbols('SPY')  
spy<-as.data.frame(Cl(SPY))

spy$date<-as.Date(rownames(spy),"%Y-%m-%d")

spy$ema50 <- EMA(Cl(SPY),n=50,wilder = TRUE) / Cl(SPY)

spy$slope50 <- NA

for(i in 1:nrow(spy)) {
        rng <- max(1,i-50):i
        spy[i,'slope50'] <- lm(SPY.Close ~ dt,
                          data.frame(dt=rng, close=Cl(SPY[rng]))
                          )$coefficients[2]
}


margin<-100
work<-spy[(margin+1):(length(spy$date)-margin),, drop=FALSE]
#work$price <- spy[(margin+1):(length(spy$date)-margin),"SPY.Close"]
work$month.later <- spy[(2*margin+1):(length(spy$date)),"SPY.Close"] /
        spy[(margin+1):(length(spy$date)-margin),"SPY.Close"]


for(i in 1:margin)
        eval(parse(text=paste0("work$d",
                               i,
                               '<-spy[((margin+1):(length(spy$date)-margin))-',
                               i,
                               ',"SPY.Close"]')))

for(i in 1:margin)
        work[,i+5] <- round(work[,i+5]/work[,1],2)

work$doy <- as.numeric(strftime(work$date, format = "%j"))
work$dow <- as.numeric(strftime(work$date, format = "%w"))
work$woy <- as.numeric(strftime(work$date, format = "%W"))

work2 <- work#[,-2]

library(caret)
#inTrain <- createTimeSlices(work$month.later, 30)
inTrain <- createDataPartition(work2$month.later, p=0.8, list=FALSE)
trainer <- work2[inTrain,]
tester <- work2[-inTrain,]

inSeek <- createDataPartition(trainer$month.later, p=0.8, list=FALSE)
dis <- trainer[inSeek,]
eva <- trainer[-inSeek,]

dis<-dis[,-(1:2)]


#mod <- train(month.later ~ ., data=dis, method="rpart")
#plot(mod$finalModel); text(mod$finalModel)
# If grew more than 5.8% compared to 16 days ago, will grow 12% in a month
# Otherwise, if didn't grew 2.56% to 100 days ago, and after March 20, will lose
# But if after March, and lost 14% compared to 50 days ago, then will gain 30%

# mod <- train(month.later ~ ., data=dis, method="rf", ntree=5,
#              trControl=trainControl(method="oob"))
# 
# confusionMatrix(eva$month.later>1, predict(object = mod, newdata = eva)>1)

# 75%
mod.rpart <- train(month.later ~ ., data=dis, method="rpart")
mod.rf <- train(month.later ~ ., data=dis, method="rf",  #ntree=50,
             trControl=trainControl(method="oob", verbose=TRUE))
mod.nn <- train(month.later ~ ., data=dis, method="nnet")
mod <- train(month.later ~ .,
             data=data.frame(month.later=dis$month.later,
                             k.rpart=predict(mod.rpart, dis),
                             k.rf=predict(mod.rf,dis),
                             k.nn=predict(mod.nn,dis)
             ), method="rf")


eva.backup <- eva
DF <- function(t) {
        data.frame(month.later=t$month.later,
                             k.rpart=predict(mod.rpart, t),
                             k.rf=predict(mod.rf,t),
                             k.nn=predict(mod.nn,t)
)}

              
             

confusionMatrix(eva$month.later>1, predict(object = mod, newdata = eva)>1)

# guessed<-(eva$month.later>1)&(predict(object = mod, newdata = eva)>1)
# plot(eva$date,guessed)

library(ggplot2)

eva2<-eva 
#eva<-eva2[eva2$date<as.Date('2015-01-01','%Y-%m-%d') & eva2$date>as.Date('2014-01-01','%Y-%m-%d'),]
eva<-eva2[eva2$date>as.Date('2015-01-01','%Y-%m-%d'),]
gf <- function(x) x >1
guessed <- gf(eva$month.later) == gf(predict(object = mod, newdata = eva))
# how much reality better than prediction, color marks if market grew
#qplot(eva$date,eva$month.later-predict(object = mod, newdata = eva),color=gf(eva$month.later),size=5)
ggplot()+
        geom_line(aes(eva$date,eva$month.later)) +
        geom_line(aes(eva$date,predict(object = tre, newdata = eva)), color="red")

nne <- train(month.later ~ ., data=dis, method="nnet")

varImpPlot(mod$finalModel, main="Most influencial")

#tre <- train(month.later ~ ., data=dis, method="rpart")
tre <- rpart(month.later ~ ., data=dis)

###############################

sig<-ifelse(predict(object = mod, newdata = work2)>1.1, 1, 0)
roc0 <- ROC(Cl(SPY))
roc <- roc0[(margin+1):(length(spy$date)-margin)]
ret <- roc*sig
# install.packages("PerformanceAnalytics")
require(PerformanceAnalytics)
table.Drawdowns(ret, top=10)
# create table of downside risk estimates
table.DownsideRisk(ret)
# chart equity curve, daily performance, and drawdowns
charts.PerformanceSummary(ret)

#### But these models based on daily, while model assumed you keep for month
sig<-ifelse(predict(object = mod, newdata = DF(work2))>1.1, 1, 0)
ret <- roc*sig

money <- 10000
fee <- 10
stock <- 0
assets <- numeric(nrow(work))
ret <- numeric(nrow(work))
ret[1] <- 1
days.counter <- 0
for(i in 1:nrow(work)) {
        if(sig[i] == 1 & money > 0) {
                stock <- (money - fee) / work[i,"SPY.Close"]
                money <- 0
        }
        if(sig[i] == 0 & stock > 0) {
                ifelse(days.counter==30, {
                        money <- stock * work[i,"SPY.Close"] - fee
                        stock <- 0
                        days.counter <- 0
                }, days.counter <- days.counter + 1)
        }
        assets[i] <- max(money, stock * work[i,"SPY.Close"])
        if(i > 1) ret[i] <- assets[i]/assets[i-1]
}
names(assets)<-work$date
#plot(assets)
qplot(work$date,assets)      
# This model produces 12.57% average annual over last 7 years
(assets['2014-12-31']/assets['2007-12-31'])^(1/7)
