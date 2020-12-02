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

#what variables make sense? cylinders/weight correlated. heavier => more cylinders
#look into data before we exclude anything.
#could you even make cylinders continuous?
#highly correlated =>remove.
#don't remove w/o justification.
#try to justify w/ horsepower, making that continuous.
#approximately satisfy assumptions


#PCA used when there's multicollinearity
#by design orthogonal

#WRITE UP YOUR PART FOR REPORT AND PRESENTATION BY THURSDAY
#Matt P: model selection, transformation, ridge/lasso.
lmod <- lm(mpg~  displacement + horsepower + weight + acceleration, data=mpgdata)
plot(lmod$residuals ~ lmod$fitted.values)

#what does R do with NA's on horsepower
#cylinders wasn't significant???

#Box-Cox transformation, full model
boxcox(lmod, plotit=T)
bclmod <- lm(-2*(sqrt(mpg)-1)~  displacement + horsepower + weight + acceleration, data=mpgdata)
plot(bclmod$residuals ~ bclmod$fitted.values)

#Subset selection
summary(regsubsets(-2*(sqrt(mpg)-1)~displacement + horsepower + weight + acceleration, data=mpgdata))
OneVarMod <- lm(-2*(sqrt(mpg)-1)~weight,data=mpgdata)
TwoVarMod <- lm(-2*(sqrt(mpg)-1)~horsepower+weight, data=mpgdata)
ThreeVarMod <- lm(-2*(sqrt(mpg)-1)~displacement+horsepower+weight,data=mpgdata)

#horsepower vs. acceleration?
#e.g. vif less than 4, more than 4. no hard and fast rule.
#bring in other practical considerations. ideally vif should be close to 1. 1.3 vs 1.5 not a big difference.
paste("adjusted R^2 for model with ",c(1,2,3,4),"predictors: ",
      c(summary(OneVarMod)$adj.r.squared,summary(TwoVarMod)$adj.r.squared,
        summary(ThreeVarMod)$adj.r.squared,summary(bclmod)$adj.r.squared))

paste("AIC for model with ",c(1,2,3,4),"predictors: ",
      c(AIC(OneVarMod),AIC(TwoVarMod),
        AIC(ThreeVarMod),AIC(bclmod)))
#chose 2 variable mod
anova(TwoVarMod,bclmod)
anova(ThreeVarMod,bclmod)

#state how you deal with missing data. only 6 missing values and don't want to add extra uncertainty
#from imputation.

#how many missing observations is a lot? e.g. more than 20% missing data gets tossed out entirely.
#context dependent.

#check constant variance
plot(TwoVarMod$residuals ~TwoVarMod$fitted.values)

#is this satisfactory for constant variance? are all the ups and downs on left side of plot
#because there are more observations there?

#clearly the errors ought to be independent...not time series, etc.
#but is it? model year = year car was made....

#check normal data
qqnorm(residuals(TwoVarMod),ylab = "Residuals",main="")
qqline(residuals(TwoVarMod))
shapiro.test(residuals(TwoVarMod))

#check for influential points: Cook's distance
plot(TwoVarMod) #there do not seem to be any? What's high cook's distance?

#checking for outliers:
range(rstudent(TwoVarMod))
which(abs(rstudent(TwoVarMod))>=3)

NoOutlierData <- mpgdata[-c(322,387,320,381),]
BestLmod <- lm(-2*(sqrt(mpg)-1)~horsepower+weight, data=NoOutlierData)
plot(BestLmod)
AIC(BestLmod)
AIC(TwoVarMod)
#better model without the outliers

#checking linear relationships:
lmod2 <- lm(mpg ~horsepower + weight, data=NoOutlierData)
delta_hp <- residuals(lm(horsepower ~ weight, data=NoOutlierData))
delta_wgt <- residuals(lm(weight~horsepower, data=NoOutlierData))
plot(residuals(lmod2)~delta_hp)
plot(residuals(lmod2)~delta_wgt)

#all over the place, clearly not linear... 
#do I do this on the transformed data or original data?

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








