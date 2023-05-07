library(psych)
library(ggplot2)
library(moments)
library(qqplotr)
ram <- mobiles$ram

mean(ram)
median(ram)

var(ram)
sd(ram)
sd(ram)/mean(ram)*100
max(ram)-min(ram)
IQR(ram)

boxplot(ram, main="Box-and-whiskers plot of Random Access Memory", 
        ylab=" In Megabytes", col="darkred")

summary(ram)
quantile(ram)

quantile(ram, probs=seq(0.1,0.9))[c(1,9)]

skewness(ram)
kurtosis(ram)

hist(ram, freq=FALSE, col = "darkred")
lines(density(ram), col='black', lwd=3)

hist(ram, breaks = "Scott", freq=FALSE , col = "brown")
lines(density(ram), col='black', lwd=3)

hist(ram, breaks = "FD", freq=FALSE, col = "darkgoldenrod1")
lines(density(ram), col='red', lwd=3)

qqnorm(ram, frame=FALSE, col="brown")
qqline(ram, lwd=2, col= "black")

ggplot(mobiles, aes(sample=ram, col="red")) + stat_qq()
dp<-list(mean=mean(ram), sd=sd(ram))
ggplot(mobiles, mapping=aes(sample=ram, col="blue")) + stat_pp_band(distribution = "norm", dparams = dp) +
  stat_pp_line() +stat_pp_point(distribution = "norm", dparams = dp)

shapiro.test(ram)
