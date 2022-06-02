train<-read.csv("C:\\Studia\\MAGISTERKA\\2. semestr\\Warsztaty\\dane\\train.csv")
head(train)

train<-train[,-1]
str(train)
train$date<-as.Date(train$date)
train2<-train[train$date<as.Date('2016-06-01'),]
test2<-train[train$date>=as.Date('2016-06-01'),]

train2_mean<-aggregate(train2$sales, list(train2$date), FUN=mean)
head(train2_mean)

plot(train2_mean$x~train2_mean$Group.1,type="l")


ts<-ts(train2_mean$x)
head(ts)
ts.plot(ts)

acf(ts) #widac sezonosc i trend
ts_diff<-diff(ts)

ts.plot(ts_diff)
acf(ts_diff) #pozbylismy siê trendu, ale zostala sezonowosc
pacf(ts_diff)

#wyznaczmy okres
spec<-spectrum(ts_diff)
#widac ze nie jest to bialy szum
#wyznaczmy okres
cpgram(ts_diff)

(spec$freq[order(-spec$spec)[1:2]])
# 0.2864583 0.1435185

#czyli okresy
1/(spec$freq[order(-spec$spec)[1:2]])
#4(takie 3.5) i 7

1/spec$freq[which.max(spec$spec)] #oko³o 7 przy freq=1
#0.0190897 przy freq=365
#czyli okres to oko³o tydzieñ

okres<-ceiling(1/spec$freq[which.max(spec$spec)])
okres

#roznicujemy o okres
ts_diff2<-diff(ts_diff,lag=7)

acf(ts_diff2)
pacf(ts_diff2,lag.max=100)

#jedno roznicowanie z lagiem -> D=1
#jedno zwykle roznicowanie => d=1
#z acf(ts_diff2) bysmy moze wzieli q=6 alob q=2, Q=1
#z pacf  => p=6, P=5
ar(ts_diff2)
30/7

sarima<-arima(ts,order=c(7,1,2),seasonal=list(order=c(8,1,6),period=7))
?arima
#AIC(arima(ts,order=c(6,1,6),seasonal=list(order=c(6,1,1),period=7)))
Box.test(sarima$residuals,lag=round(sqrt(length(sarima$residuals))),type="Ljung-Box",fitdf=22)
#p-value = 8.818e-05 => nie jest to bialy szum

acf(sarima$residuals)
pacf(sarima$residuals,lag.max=100)






