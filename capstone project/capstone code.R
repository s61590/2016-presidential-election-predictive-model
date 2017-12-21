#Importing the data
df<- read.csv("us2016electiondata.csv")
#Summary statistics for the variables in the data
summary(df)

#Creating a subset of the data that contains all continuous variables for ease of
#analysis. percent_democreat and pecent_democrat_2012 are excluded from this subset
#since they are highly linearly correlated with percent_republican and percent_
#republican 2012(ie %R + %D=100%, excluding the 3rd party vote share)
df2<-subset(df, select=c("average_age", "percent_republican_2012","percent_white",
"percent_uninsured", "percent_degree", "percent_unemployed", "average_income",
 "percent_republican"))
#Installing packages
install.packages("mice")
library(mice)
install.packages("tidyverse")
library(tidyverse)
install.packages("caTools")
library(caTools)
install.packages("dplyr")
library(dplyr)

#Dividing df2 dataset into train and test sets (80% in train, 20% in test)
# Predictive model will be built on train set and then tested on test set.
set.seed(1000)
split=sample.split(df2$percent_republican, SplitRatio=0.80)
train=subset(df2, split == TRUE)
test=subset(df2, split == FALSE)

#Initial linear regression model on train data set: Contains all 7 explanatory 
#independent variables. na.omit used to exclude observations with missing values
model1<-lm(percent_republican ~ percent_republican_2012 + average_income + 
average_age +percent_white + percent_uninsured + percent_degree  + percent_unemployed,
data=na.omit(train)) 
summary(model1) 

#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              8.511e+00  9.118e-01   9.333  < 2e-16 ***
#  percent_republican_2012  7.916e-01  6.854e-03 115.485  < 2e-16 ***
#  average_income           3.600e-05  1.635e-05   2.202   0.0278 *  
#  average_age              1.283e-01  1.532e-02   8.375  < 2e-16 ***
#  percent_white            1.545e-01  5.734e-03  26.943  < 2e-16 ***
#  percent_uninsured        2.839e-02  1.918e-02   1.481   0.1388    
#  percent_degree          -4.816e-01  9.867e-03 -48.810  < 2e-16 ***
#  percent_unemployed      -2.247e-01  2.771e-02  -8.109 7.93e-16 ***
  ---
#In model 1, all the explanatory variables are significant at an alpha
#=0.05 level, except for percent_uninsured, which has a p-value of 0.139. The adjusted
#R-squared for this model was 0.9587.
#Calculating SSE and RMSE for initial linear regression model
model1data <- na.omit(select(train, percent_republican, percent_republican_2012,
  average_income, average_age, percent_white, percent_uninsured, percent_degree,
  percent_unemployed))
SSE = sum(model1$residuals^2) # sum of squared error=24748.3
SSE
RMSE = sqrt(SSE/(nrow(model1data))) # root mean squared error=3.15
RMSE
# Creating a correlation matrix to look for collinearity
cor(df2, use="complete.obs") #will omit obserations with missing values
#Correlation coefficients between percent_republican and each explanatory variable
#average_age                      0.3325677            
#percent_republican_2012          0.9347904
#percent_white                    0.5365550    
#percent_uninsured                0.1950089            
#percent_degree                  -0.4936432             
#percent_unemployed              -0.2728920            
#average_income                  -0.1996266   

# Among the 7 independent explanatory variables in our intial model, the pairs of 
# variables with the highest degree of collinearity are: 
# 1) average_age and percent_white (r=0.456)
# 2) percent_republican_2012 and percent_white(r=0.433)
# 3) percent_white and percent_uninsured(r=-0.473) 
# 4) percent_uninsured and average income (r=-0.436)
# 5) percent_degree and average income (r=0.51)
#Given that average_income is negatively correlated with percent_repulican
#but has a positive coefficient in our regression model, there is likely
#high degree of collinearity with other variables. Indeed, average_income is highly
#correlated with percent_uninsured and percent_degree. Given that average_income
#is not highly correlated with percent_republican (r=-0.1996) anyway, our next
#model will remove average_income. 
model_income_removed<-lm(percent_republican ~ percent_republican_2012 + percent_degree + 
average_age +percent_white + percent_unemployed +percent_uninsured, data=na.omit(train))
summary(model_income_removed)
#                         Estimate   Std. Error t value Pr(>|t|)    
#(Intercept)              9.412875   0.815182  11.547  < 2e-16 ***
# percent_republican_2012  0.795446   0.006632 119.936  < 2e-16 ***
# percent_degree          -0.471674   0.008788 -53.673  < 2e-16 ***
# average_age              0.131899   0.015244   8.653  < 2e-16 ***
# percent_white            0.151230   0.005544  27.281  < 2e-16 ***
# percent_unemployed      -0.221358   0.027687  -7.995 1.96e-15 ***
# percent_uninsured        0.010694   0.017424   0.614    0.539    
#In this model, all the variables have the right sign, but percent_uninsured is not
#signiifiant likely due to collinearity probems. Adusted R2 is 0.9587 in this model.
#Since percent_uninsured is still not significant, has high collinearity with other
#variabes and is poorly correlated with percent_republican, our next model will remove
#percent_uninsured.
# Creating new model with both percent_uninsured and average_income removed.
model_income_and_uninsured_removed<-lm(percent_republican ~ percent_republican_2012 + 
percent_degree + average_age +percent_white + percent_unemployed, data=na.omit(train))
summary(model_income_and_uninsured_removed)
#This model is a good model. After removing two variables due to problems with 
#collinearity, our simplified 5 variable moddel has an adjusted R2 that has remained
#onstant at a high value of 0.9587. All five variables are highl significant as seen 
#in this table of the model coefficients: 
#                          Estimate   Std. Error t value  Pr(>|t|)    
#(Intercept)               9.579112   0.768778   12.460   < 2e-16 ***
# percent_republican_2012  0.797708   0.005513   144.697  < 2e-16 ***
  #percent_degree          -0.472347   0.008718  -54.180  < 2e-16 ***
  #average_age              0.133391   0.015046   8.865    < 2e-16 ***
  #percent_white            0.148985   0.004164  35.775    < 2e-16 ***
  #percent_unemployed      -0.218614   0.027320  -8.002    1.86e-15 ***
  #All five variables are highly significant and have the right sign.
#Calculating SSE and RMSE
final_model_data <- na.omit(select(train, percent_republican, percent_republican_2012,
average_age, percent_white, percent_degree,percent_unemployed))
SSE = sum(model_income_and_uninsured_removed$residuals^2) 
SSE=24800.4
RMSE = sqrt(SSE/(nrow(final_model_data))) 
RMSE=3.156
#SSE and RMSE have increased negligibly from our initial model (model1).
#Plotting the model to check for linear regression assumptions
plot(model_income_and_uninsured_removed)
#This will be our final model to use for prediction on the test data set.
library(dplyr)
library(tidyverse)
#Using the mutate and predict functions to create a new variable of predicted 
#values for Republican share of the vote in the test data set
predict_republican_test<-mutate(test, predict_republican=predict
                                (model_income_and_uninsured_removed, newdata=test))
#Summary statisitcs for the new dataset containing the new variable of predicted values
summary(predict_republican_test)
#Testing the accuracy of our predictive model by correlating our predicted values 
#with the actual values.
cor(predict_republican_test, use="complete.obs")
#Correlation value R between predict_republican_test and percent_republican is 0.974.



