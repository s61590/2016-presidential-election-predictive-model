#Importing the data
df<- read.csv("us2016electiondata.csv")
summary(df)
cor.test
#Creating a subset of the data that contains all continuus variables
df2<-subset(df, select=c("average_age", "percent_republican_2012","percent_white",
"percent_uninsured", "percent_degree", "percent_unemployed", "average_income",
"percent_democrat", "percent_republican", "percent_democrat_2012"))

#Installing packages
install.packages("mice")
library(mice)
install.packages("tidyverse")
library(tidyverse)
install.packages("caTools")
library(caTools)
install.packages("dplyr")
library(dplyr)
#Dividing df2 dataset into train and test sets
set.seed(1000)
split=sample.split(df2$percent_republican, SplitRatio=0.80)
train=subset(df2, split == TRUE)
test=subset(df2, split == FALSE)
#Initial linear regression model: Contains all explanatory independent variables
model1<-lm(percent_republican ~ percent_republican_2012 + average_income + 
average_age +percent_white + percent_uninsured + percent_degree  + percent_unemployed,
data=na.omit(train))
summary(model1) #In model 1, all the explanatory variables are significant at an alpha
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
# Among the 7 independent explanatory variables in our intial model, the pairs of 
# variables with the highest degree of collinearity are: 
# 1) average_age and percent_white (r=0.456)
# 2) percent_republican_2012 and percent_white(r=0.433)
# 3) percent_white and percent_uninsured(r=-0.473) 
# 4) percent_uninsured and average income (r=0.436)
# 5) percent_degree and average income (r=0.51)
#Given that percent_uninsured is in 2 of the five pairs with the highest degree of 
#collinearity and is the only non-significant variable in the initial model, our 
#next model, model 2, will omit percent_uninsured.

model2<-lm(percent_republican ~ percent_republican_2012 + average_income + 
             average_age +percent_white + percent_degree + percent_unemployed
           , data=na.omit(train))
summary(model2)
#Calculating SSE and RMSE for model2
model2data <- na.omit(select(train, percent_republican, percent_republican_2012,
  average_income, average_age, percent_white, percent_degree,percent_unemployed))
SSE = sum(model2$residuals^2) # sum of squared error=24748.3
SSE
RMSE = sqrt(SSE/(nrow(model2data))) # root mean squared error=3.15
RMSE
# In model 2, all of the 6 explanatory variables are significant except for 
#average_income, which has a p value of 0.0818. The adjusted R2 for this model is the
#same as for model 1, 0.9587. Going frm model 1 t model 2, SSE has increased slightly
#from 24748 to 24770 and RMSE has increased slightly from 3.153 to 3.154. However, 
#this slight increase in error is relatively insignficant, so model 2 is a better model
#due to containing one less variable. 
#Since Since average_income is the only insignificant variable in this model and has
#high degree of collinearity with percent_degree, let us remove average_income from our
#model and test this new model. 
model3<-lm(percent_republican ~ percent_republican_2012 + percent_degree
             + average_age +percent_white + percent_unemployed, data=train)
summary(model3)
model3data <- na.omit(select(train, percent_republican, percent_republican_2012,
                      average_age, percent_white, percent_degree,percent_unemployed))
SSE = sum(model3$residuals^2) 
SSE
RMSE = sqrt(SSE/(nrow(model3data))) 
RMSE
#In model 3, SSE and RMSE have increased dramatically to 48503 and 4.41 respectively.
#In addition, the adjusted R-squared has decreased to 0.9192. All the variables in model
# 3 are significant, but due to the increased error and decreased R-squared, the 
# previous model, model2, is a better model.

#Another look at model 2: Making sure the signs in model 2 correlate with the sign of the
#correlation coefficient between percent_republican and each explanatory variable
cor(df2, use="complete.obs")
#Correlation coefficients between percent_republican and each explanatory variable
#average_age                      0.3325677            
#percent_republican_2012          0.9347904
#percent_white                    0.5365550    
#percent_uninsured                0.1950089            
#percent_degree                  -0.4936432             
#percent_unemployed              -0.2728920            
#average_income                  -0.1996266            
summary(model1)
#Looking at model 1, average_income has a positive coefficient, indicating collinearity
#problems. All other variables have right sign. Adjusted R2 0.9587 in model 1 
model_income_removed<-lm(percent_republican ~ percent_republican_2012 + percent_degree + 
  average_age +percent_white + percent_unemployed + percent_uninsured, data=train)
summary(model_income_removed)
#In this model, all the variables have the right sign, but purcent_uninsured is not
#signiifiant likely due to collinearity probems. Adusted R2 is 0.9587 in this mode.

# Creating new model with both percent_uninsured and average_income removed.
model_income_and_uninsured_removed<-lm(percent_republican ~ percent_republican_2012 + 
  percent_degree + average_age +percent_white + percent_unemployed, data=train)
summary(model_income_and_uninsured_removed)
#This model is a good model. After removing two variables due to problems with 
#collinearity, our simplified 5 variable moddel has an adjusted R2 that has remained
#onstant at a high value of 0.9587. All five variables are highl significant as seen 
#in this table of the model coefficients: 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)              9.579112   0.768778  12.460  < 2e-16 ***
  percent_republican_2012  0.797708   0.005513 144.697  < 2e-16 ***
  percent_degree          -0.472347   0.008718 -54.180  < 2e-16 ***
  average_age              0.133391   0.015046   8.865  < 2e-16 ***
  percent_white            0.148985   0.004164  35.775  < 2e-16 ***
  percent_unemployed      -0.218614   0.027320  -8.002 1.86e-15 ***
  ---
  All five variables have the right sign.
#Calculating SSE and RMSE
final_model_data <- na.omit(select(train, percent_republican, percent_republican_2012,
                             average_age, percent_white, percent_degree,percent_unemployed))
SSE = sum(model_income_and_uninsured_removed$residuals^2) 
SSE=24800.4
RMSE = sqrt(SSE/(nrow(final_model_data))) 
RMSE=3.156
#This will be our final model to use for prediction on the test data set.
library(dplyr)
library(tidyverse)
predict_republican_test<-mutate(test, predict_republican=predict
(model_income_and_uninsured_removed, newdata=test))
summary(predict_republican_test)
cor(predict_republican_test, use="complete.obs")
#Correlation value R between predict_republican_test and percent_republican is 0.974.