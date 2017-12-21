library(ggplot2)
library(readr)
df<-read.csv("us2016electiondata.csv")
#Creating an initial scatter plot of percent_republican vs. percent_republican_2012
ggplot(df) + aes(x = percent_republican_2012, y = percent_republican) +geom_point()
#Creating an initial scatter plot of percent_republican vs. average_age
ggplot(df) + aes(x = average_age, y = percent_republican) +geom_point()
#Creating an initial scatter plot of percent_republican vs. percent_white
ggplot(df) + aes(x = percent_white, y = percent_republican) +geom_point()
#Creating an initial scatter plot of percent_republican vs. percent_uninsyred
ggplot(df) + aes(x = percent_uninsured, y = percent_republican) +geom_point()
#Creating an initial scatter plot of percent_degree vs. percent_republican_2012
ggplot(df) + aes(x = percent_degree, y = percent_republican) +geom_point()
#Creating an initial scatter plot of percent_unemployed vs. percent_republican_2012
ggplot(df) + aes(x = percent_unemployed, y = percent_republican) +geom_point()
#Creating an initial scatter plot of average_income vs. percent_republican_2012
ggplot(df) + aes(x = average_income, y = percent_republican) +geom_point()


#Looking at effects of race by adding "color=percent_white"
ggplot(df) + aes(x = percent_republican_2012, y = percent_republican, color = percent_white)+
geom_point()
#Looking at effects of age
ggplot(df) + aes(x = percent_republican_2012, y = percent_republican, color=average_age)+
  geom_point()
#Looking at effects of being uninsured
ggplot(df) + aes(x = percent_republican_2012, y = percent_republican, color=percent_uninsured)+
  geom_point()
#Looking at effect of college degree
ggplot(df) + aes(x =percent_republican_2012, y=percent_republican, color=percent_degree)+
  geom_point()
#Looking at effect of unemployment
ggplot(df) + aes(x =percent_republican_2012, y=percent_republican, color=percent_unemployed)+
  geom_point()
ggplot(df) + aes(x =percent_republican_2012, y=percent_republican, color=average_income)+
  geom_point()
#Creating an initial scatter plot of percent_republican vs. percent_republican_2012
ggplot(df) + aes(x = percent_republican_2012, y = percent_republican) +geom_point()