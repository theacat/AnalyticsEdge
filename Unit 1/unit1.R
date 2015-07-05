###Video4 
WHO <- read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_EUROPE <- subset(WHO, Region=="Europe")
str(WHO_EUROPE)
summary(WHO_EUROPE)
write.csv(WHO_EUROPE,"WHO_Europe.csv")
ls()
rm(WHO_EUROPE)
ls()
### Video5
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
minCol <- which.min(WHO$Under15)
WHO$Country[minCol]
maxCol <-which.max(WHO$Under15)
WHO$Country[maxCol]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO,GNI>10000 & FertilityRate > 2.5)
nrow(Outliers)
#找出 Outliers 
Outliers[c("Country", "GNI","FertilityRate")]
hist(WHO$CellularSubscribers)
###boxplot 中間黑線是median, box 是1st~3rd 分衛，曲線叫 whisters 秀出最大與最小值。小圓圈是 outlier 
boxplot(WHO$LifeExpectancy~WHO$Region)
boxplot(WHO$LifeExpectancy~WHO$Region,xlab="",ylab="LifeExpectancy", main="Life LifeExpectancies of Countires by Region")
table(WHO$Region)
### tapply 把2nd argument 的Data分群，然後使用3rd arg 的 func 做處理
tapply(WHO$Over60,WHO$Region,mean)
### Outcome 很多都是 NA , 這是因為有 NA 值
tapply(WHO$LiteracyRate, WHO$Region, min)
### 把 NA 值去掉
tapply(WHO$LiteracyRate, WHO$Region, min,na.rm=TRUE)



