library(leaps)
library(MASS)
library(dplyr)
library(ggstance)
library(cowplot)
library(tidytree)
library(ggplot2) 


# Data loading
BodyFat=read.csv("BodyFat.csv",header = T)
attach(BodyFat)
par(mfrow=c(2,2))

# Histograms
#Bodyfat
hist(BODYFAT,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="BODYFAT %")
#AGE
hist(AGE,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="AGE")
#Weight
hist(WEIGHT,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="WEIGHT")
#HEIGHT
hist(HEIGHT,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="HEIGHT")
#NECK
hist(NECK,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="NECK")
#CHEST
hist(CHEST,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="CHEST")
#ABDOMEN
hist(ABDOMEN,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="ABDOMEN")
#HIP
hist(HIP,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="HIP")
#THIGH
hist(THIGH,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="THIGH")
#KNEE
hist(KNEE,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="KNEE")
#ANKLE
hist(ANKLE,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="ANKLE")
#BICEPS
hist(BICEPS,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="BICEPS")
#FOREARM
hist(FOREARM,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="FOREARM")
#WRIST
hist(WRIST,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="WRIST")

# Data Cleaning
BodyFat=subset(BodyFat,BodyFat$BODYFAT>0.5&BodyFat$BODYFAT<42.3)
BodyFat=subset(BodyFat,BodyFat$WEIGHT<300)
BodyFat=subset(BodyFat,BodyFat$HEIGHT>50)
BodyFat=subset(BodyFat,BodyFat$ABDOMEN<140)
BodyFat=subset(BodyFat,BodyFat$HIP<140)
BodyFat_clean=subset(BodyFat,BodyFat$THIGH<80)
BodyFat_clean$WEIGHT=BodyFat_clean$WEIGHT*0.453592
colnames(BodyFat_clean)
dim(BodyFat_clean)

# Find the model
# Find best predictor subsets
best_subset=regsubsets(BODYFAT~AGE+WEIGHT+HEIGHT+NECK+CHEST+ABDOMEN+HIP
                       +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = BodyFat_clean,
                       nbest = 1,nvmax = 12,force.in = NULL,force.out = NULL,
                       method = "exhaustive")
summary_of_best_subset=summary(best_subset)
summary_of_best_subset
as.data.frame(summary_of_best_subset$outmat)
plot(best_subset,scale = "r2")

reg.summary = summary_of_best_subset
par(mfrow=c(2,2))

# Plot the statistics for subsets
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab=expression(R^2),type="l")
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')

# Regression Model
lmmodel=lm(BODYFAT~WEIGHT+ABDOMEN+WRIST,data=BodyFat_clean)
summary(lmmodel)

# summary
# standardized residual plot
par(mfrow = c(1,1))
plot(predict(lmmodel),resid(lmmodel),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")

# qqnorm plot
qqnorm(rstandard(lmmodel),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

# Prediction vs actural value plot
p1 = ggplot(BodyFat_clean, aes(x=predict(lmmodel), y=BodyFat_clean$BODYFAT)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')
p1
