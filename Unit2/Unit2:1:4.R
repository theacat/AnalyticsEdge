setwd("~/Documents/R簡報與教學/201506_RedX")
wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 = lm(Price ~ HarvestRain, data = wine)
summary(model1)

model2 = lm(Price ~ HarvestRain+WinterRain, data = wine)
summary(model2)
cor(wine$HarvestRain, wine$WinterRain)


