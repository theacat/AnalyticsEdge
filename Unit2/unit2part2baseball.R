baseball = read.csv("baseball.csv")
moneyball = subset(baseball, Year < 2002)
moneyball$RD = moneyball$RS-moneyball$RA
model1 = lm(W ~RD , data = moneyball)
summary(model1)

model2 = lm(RS ~ OBP + SLG , data = moneyball)
summary(model2)
0.361*2737.77 + 0.5*1584.91 -804.63
0.369*2737.77 + 0.374*1584.91 -804.63


model3 = lm(RA ~ OOBP + OSLG , data = moneyball)
summary(model3)
0.297*2913.60 + 0.370*1514.29 -837.38

predict(model2, newdata = data.frame(OBP= c(0.361,0.369), SLG=c(0.5,0.374)))




teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)
