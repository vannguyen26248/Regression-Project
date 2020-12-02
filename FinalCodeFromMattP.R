setwd("C:/Users/Matt/Desktop/regression project/Regression-Project")
rm(list=ls())
library(faraway)
library(MASS)
library(leaps)
library(RcmdrMisc)
mpgdata <- na.omit(read.table("auto-mpg.data",header= TRUE, 
                      col.names = c("mpg","cylinders", "displacement", 
                                    "horsepower","weight", "acceleration",
                                    "model_year", "origin", "car_name"),na.strings="?"))
lmod <- lm(mpg~  displacement + horsepower + weight + acceleration, data=mpgdata)



summary (lmod)
plot(lmod$residuals ~lmod$fitted.values, xlab="Fitted Values",ylab="Residuals", main="
     First Model: Residuals vs. Fitted Values")
#constant variance assumption isn't satisfied, fan shape


#chose to do a transformation to get more constant variance over weighted least squares.


#Box-Cox transformation, full model
boxcox(lmod, plotit=T)
boxcoxmod <- lm(-2*(sqrt(mpg)-1)~  displacement + horsepower + weight + acceleration, data=mpgdata)
plot(boxcoxmod$residuals ~ boxcoxmod$fitted.values,xlab="Fitted Values",ylab="Residuals", main="
     Box-Cox Model: Residuals vs. Fitted Values")





#log transformation, full model
logmod <- lm(log(mpg) ~ displacement + horsepower + weight + acceleration, data=mpgdata)
plot(logmod$residuals ~ logmod$fitted.values, xlab="Fitted Values",ylab="Residuals", main="
     Log Model: Residuals vs. Fitted Values")
#constant variance is better, no influential points


#check for normality
qqnorm(residuals(logmod),ylab = "Residuals",main="qq-plot After Log Transformation")
qqline(residuals(logmod))
shapiro.test(residuals(logmod))


#check for outliers
range(rstudent(logmod))
rstudent(logmod)[which(abs(rstudent(logmod))>=3)] #see which studentized residuals >=3,
#observations 111 and 387 are outliers


halfnorm(hatvalues(logmod), main="Half-Normal Plot: Leverages")
#some observations need investigation
halfnorm(cooks.distance(logmod),ylab="Cook's Distance", main="Half-Normal Plot: Cook's Distances") #faraway
#no cook's distances need investigation... no potential influential points

#remove the outliers, and see what happens; need to see what happens when we remove

NoOutlierData <-mpgdata[-which(abs(rstudent(logmod))>=3),]

logmodNoOutliers <- lm(log(mpg) ~ displacement + horsepower + weight + acceleration, data=NoOutlierData)


#variable selection
stepwise(logmod,direction="backward/forward",criterion="AIC") #RcmdrMisc
stepwise(logmod,direction="forward/backward",criterion="AIC")
stepwise(logmodNoOutliers,direction="backward/forward",criterion="AIC")
stepwise(logmodNoOutliers,direction="forward/backward",criterion="AIC")

#subset selection, with outliers then without outliers
summary(regsubsets(log(mpg) ~ displacement + horsepower + weight + acceleration, data=mpgdata))
summary(regsubsets(log(mpg) ~ displacement + horsepower + weight + acceleration, data=NoOutlierData))
#best models of each size are the same
ModelScores <- function(Dataset,lmodel){
  
  OneVarMod <- lm(log(mpg)~weight,data=Dataset)
  TwoVarMod <- lm(log(mpg)~horsepower+weight, data=Dataset)
  ThreeVarMod <- lm(log(mpg)~displacement+horsepower+weight,data=Dataset)
  print("Three Variable Model VIF:")
  print(vif(ThreeVarMod))
  print("Two Variable Model VIF:")
  print(vif(TwoVarMod))
  print(summary(TwoVarMod))
  
  paste("For the model with ",c(1,2,3,4),"predictors: Adjusted R^2 is ",
      c(summary(OneVarMod)$adj.r.squared, summary(TwoVarMod)$adj.r.squared,
        summary(ThreeVarMod)$adj.r.squared,summary(lmodel)$adj.r.squared),"and AIC is ",
      c(AIC(OneVarMod),AIC(TwoVarMod),
        AIC(ThreeVarMod),AIC(lmodel)))
}

#WITH outliers
ModelScores(mpgdata,logmod)
#two variable model virtually ties for best AIC and R^2, but also 
#has as many predictors as possible without collinearity


#NO outliers
ModelScores(NoOutlierData,logmodNoOutliers)
#serious multicollinearity in the three/four variable model, both variables 
#in the two variable model are significant. so we choose the two variable one.
#two variable has best AIC, very close to best R^2.


Bestmod <- lm(log(mpg)~horsepower+weight, data=NoOutlierData)
summary(Bestmod)
plot(Bestmod)
which(abs(rstudent(Bestmod))>=3)
#checking for linear relationship between transformed response and predictors
plot(log(mpg)~horsepower, data=mpgdata, main="Response vs. Horsepower")
plot(log(mpg)~weight, data=mpgdata, main="Response vs. Weight")

#both cases: nothing too concerning. multicollinearity: vifs are close but less than 4.
#note that removing outliers resulted in slightly improved R^2 and AIC, but other than that little changes.

#Conclusion: in both cases we chose model with weight and horsepower as predictors.
#in order to best satisfy model assumptions, we used the log transformation on the response.



