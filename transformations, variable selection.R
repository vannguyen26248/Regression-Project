setwd("C:/Users/Matt/Desktop/regression project/Regression-Project")
rm(list=ls())
library(faraway)
library(MASS)
library(leaps)
library(lars)
mpgdata <- read.table("auto-mpg.data",header= TRUE, 
                      col.names = c("mpg","cylinders", "displacement", 
                                    "horsepower","weight", "acceleration",
                                    "model_year", "origin", "car_name"),na.strings="?")

lmod <- lm(mpg~  displacement + horsepower + weight + acceleration, data=mpgdata)
plot(lmod$residuals ~ lmod$fitted.values)

#Box-Cox transformation, full model
boxcox(lmod, plotit=T)
bclmod <- lm(2*(sqrt(mpg)-1)~  displacement + horsepower + weight + acceleration, data=mpgdata)
plot(bclmod$residuals ~ bclmod$fitted.values)

#Subset selection
summary(regsubsets(mpg~displacement + horsepower + weight + acceleration, data=mpgdata))
OneVarMod <- lm(mpg~weight,data=mpgdata)
TwoVarMod <- lm(mpg~horsepower+weight, data=mpgdata)
ThreeVarMod <- lm(mpg~displacement+horsepower+weight,data=mpgdata)

paste("adjusted R^2 for model with ",c(1,2,3,4),"predictors: ",
      c(summary(OneVarMod)$adj.r.squared,summary(TwoVarMod)$adj.r.squared,
        summary(ThreeVarMod)$adj.r.squared,summary(lmod)$adj.r.squared))

paste("AIC for model with ",c(1,2,3,4),"predictors: ",
      c(AIC(OneVarMod),AIC(TwoVarMod),
        AIC(ThreeVarMod),AIC(lmod)))
#chose 2 variable mod
anova(TwoVarMod,lmod)
anova(ThreeVarMod,lmod)

plot(TwoVarMod$residuals ~TwoVarMod$fitted.values)
boxcox(TwoVarMod,plotit=T)
bcTwomod <- lm(2*(sqrt(mpg)-1)~  horsepower + weight , data=mpgdata)
#check constant variance
plot(TwoVarMod$residuals ~TwoVarMod$fitted.values)
#check normal data
qqnorm(residuals(bcTwomod),ylab = "Residuals",main="")
qqline(residuals(bcTwomod))
shapiro.test(residuals(bcTwomod))

#check for influential points: Cook's distance
plot(bcTwomod) #there do not seem to be any? What's high cook's distance?

#checking for outliers:
range(rstudent(bcTwomod))
which(abs(rstudent(bcTwomod))>=3)

NoOutlierData <- mpgdata[-c(322,387,320,381),]
BestLmod <- lm(2*(sqrt(mpg)-1)~horsepower+weight, data=NoOutlierData)
plot(BestLmod)
AIC(BestLmod)
AIC(bcTwomod)
#better model without the outliers

#checking linear relationships: we know they're all significant but...
delta_hp <- residuals(lm(horsepower ~ weight, data=NoOutlierData))
delta_wgt <- residuals(lm(weight~horsepower, data=NoOutlierData))
plot(residuals(BestLmod)~delta_hp)
plot(residuals(BestLmod)~delta_wgt)

#all over the place, clearly not linear... 


#Ridge regression: two variables
lambda <- seq(0, 5e-8, len=21)
rgmod <-lm.ridge(mpg ~ horsepower + weight, data=mpgdata,lambda=lambda)
BestLambda <- which.min(rgmod$GCV)
BestRgMod <- lm.ridge(mpg ~ horsepower + weight, data=mpgdata,lambda=BestLambda)
BestRgMod$coef

#lasso


#comparing OLS, Ridge, Lasso models? Do I split the data into training and testing data?
#what happens outliers wise if we do this?
#my process: variable selection goes first. then fit model, then do transformations.
#all the variables are significant though...
#vif: the two-variable model is the first one with no multicollinearity





#Matt H, principal component analysis and fixing multicollinearity

#cmpg <- mpgdata[-c(32,126,330,336,354,374),3:6]
cmpg <- mpgdata[,3:6] #edited and removed the 6 NA rows on my own file to conduct PCA
summary(cmpg)
prcmpg <- prcomp(cmpg) #not including cylinders
summary(prcmpg)
round(prcmpg$rot[,1],2)
round(apply(cmpg,2,var),2)

#scaled PCA
prcmpg2 <- prcomp(cmpg,scale=TRUE)
summary(prcmpg2)

round(prcmpg2$rot[,1],2)
round(apply(cmpg,2,var),2)

#including cylinders as one of our predictors does not matter because cylinders and displacement are related


lmod <- lm(mpg~  displacement + horsepower + weight + acceleration, data=mpgdata)
summary(lmod)
round(vif(lmod),2) ##Find VIF, serious multicollinearity
prcmpg2 <- prcomp(cmpg,scale=TRUE)
plot(prcmpg2$sdev,type="l",ylab="SD of PC", xlab="PC number")

##PC regression with first two PCs, with removal of 6 missing data
lmodpcr <- lm(mpgdata$mpg ~ prcmpg2$x[,1])
summary(lmodpcr)
round(vif(lmodpcr),2)








