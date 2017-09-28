##Analysis#3
ggplot(subset(Analytics_Data, Year %in% c(2011, 2012,2013,2014)),
       aes(x=TotalPay,
           y=BasePay,
           color=Year))+
  geom_point()
ggplot(subset(Analytics_Data, Year %in% c(2011, 2012,2013,2014)),
       aes(x=Year,
           y=BasePay,
           color= BasePay))+
  geom_point()
aes(x=Year,
    y=BasePay,
    color=Benefits))+
  geom_point()
attach(male)
attach(female)
salaryfe=female$BasePay/10
boxplot(salaryfe,xlab = "Female Salary",color="orange")
salaryma=male$BasePay/10
boxplot(salaryma,xlab = "Male Salary ",color="orange")
#bootstrap distribution#

times.Basic <- male$BasePay
times.Ext   <- female$BasePay
n.Basic <- length(times.Basic)
n.Ext <- length(times.Ext)

B <- 10^4
times.diff.mean <- numeric(B)
for (i in 1:B)
{
  Basic.boot <- sample(times.Basic, n.Basic, replace=TRUE)
  Ext.boot <- sample(times.Ext, n.Basic, replace=TRUE)
  times.diff.mean[i] <- mean(Basic.boot)-mean(Ext.boot)
}
hist(times.diff.mean, main="Bootstrap distribution of difference in means",xlab="Means")
abline(v = mean(times.diff.mean), col = "red")
abline(v = mean(times.Basic) - mean(times.Ext), col = "blue")
an = times.diff.mean[!is.na(times.diff.mean)]
times.Basi
times.diff.mec = times.Basic[!is.na(times.Basic)]
times.Ext = times.Ext[!is.na(times.Ext)]


##model##

modelbenefits=lm(Benefits~BasePay+OtherPay+TotalPay)
summary(modelbenefits)

modelbenefits2=lm(Benefits~ BasePay +OtherPay+)
summary(modelbenefits2)

#decision tree#
library(rpart)
fittree=rpart(Benefits~BasePay+OtherPay+TotalPay, data=final_data, method="class") 
plot(fittree,uniform="TRUE")
library(party)
str(Fin_Data)
decisiontree=ctree(Fin_Data$Benefits~Fin_Data$BasePay+Fin_Data$TotalPay+Fin_Data$OtherPay)
plot(decisiontree)

#clusteering#
set.seed(20)
dataCluster <- kmeans(final_data[, c(1:3)], 3, nstart = 20)
dataCluster$cluster <- as.factor(dataCluster$cluster)
ggplot(final_data, aes(BasePay, Benefits, color = dataCluster$cluster)) + geom_point()


