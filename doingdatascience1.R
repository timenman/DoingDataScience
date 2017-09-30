install.packages("doBy")

library("doBy")
setwd("C:\\Users\\Timbo\\Desktop\\R")
df <- read.csv("nyt1.csv")
head(df)

#Create a new variable, age_group, that categorizes users as "< 18", "18-24", "25-34", "35-44", "45-54", "55-64", and "65 +".

df$age_group <- cut(df$Age, c(-Inf,0, 18, 24, 34, 44, 54, 64, Inf))
summary(df)

#For a single day: Plot the distributions of number impressions and click-through-rate (CTR =# clicks/# impressions) for these six age categories.
#Define Click-Through_Rate
df$ctr <- df$Clicks/df$Impressions 

#Custom function to calculate summary statistics.
siterange <- function(arg){
  c(length(arg), min(arg), mean(arg), max(arg))
}

#For the age and age cat columns from the df dataframe, apply function siterange.
#Info on ~ character. https://stackoverflow.com/questions/14976331/use-of-tilde-in-r-programming-language
#The thing on the right of <- is a formula object. It is often used to denote a statistical model, where the thing on the left of the ~ is the response and the things on the right of the ~ are the explanatory variables. So in English you'd say something like "Species depends on Sepal Length, Sepal Width, Petal Length and Petal Width".

summaryBy(Age~age_group, data=df, FUN=siterange)

#So only signed in users have ages and genders.
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group, data = df)

install.packages("gglpot2")
library("ggplot2")
ggplot(df, aes(x=Impressions, fill=age_group))+geom_histogram(binwidth=1)

