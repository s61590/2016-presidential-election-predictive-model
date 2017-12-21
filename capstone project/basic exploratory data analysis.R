library(tidyverse)
library(readr)
df<- read.csv("us2016electiondata.csv")
#Creating a subset of the data that contains all continuus variables, except for democrat
df_continuous_without_democrat<-subset(df, select=c("average_age", "percent_republican_2012","percent_white",
                         "percent_uninsured", "percent_degree", "percent_unemployed", "average_income",
                         "percent_republican"))
summary(df_continuous_without_democrat)
str(df_continuous_without_democrat)
df2<-subset(df, select=c("average_age", "percent_republican_2012", "percent_white"))
summary(df2) 
df3<-subset(df, select=c("percent_degree", "percent_uninsured"))
summary(df3)