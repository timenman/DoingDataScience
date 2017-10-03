setwd("C:\\Users\\tenman\\Desktop\\R\\nytdata")
#TODO: Write a script that cycles through the csv. Open CSV performs the analysis, and write those larger
#conclusions to a dataframe, unload csv, read new csv, repeat.
counter = 1
dirlength <- length(list.files("C:\\Users\\tenman\\Desktop\\R\\nytdata"))
#C:\Users\tenman\Desktop\R\nytdata

#declare empty data frame

master_df <- data.frame(Age=integer(), Gender=integer(), Impressions=integer(), Clicks=integer(), Signed_In=integer(), age_group=integer(), ctr=double(), hasimps=integer(), scode=integer(), day=double())

while (counter <= dirlength)
{  df <- read.csv(paste("nyt",toString(counter),".csv", sep=""))


#Create a new variable, age_group, that categorizes users as "< 18", "18-24", "25-34", "35-44", "45-54", "55-64", and "65 +".

df$age_group <- cut(df$Age, c(-Inf,0, 18, 24, 34, 44, 54, 64, Inf))

#Create a new variable "day"

df$day <- counter

#For a single day: Plot the distributions of number impressions and click-through-rate (CTR =# clicks/# impressions) for these six age categories.
#Define Click-Through_Rate
df$ctr <- df$Clicks/df$Impressions 

#Custom function to calculate summary statistics.
siterange <- function(arg){
  c(length(arg), min(arg), mean(arg), max(arg))
}

#By age group, show me the length, min, mean, and max of ages within those categories!! THIS!
#Info on ~ character. https://stackoverflow.com/questions/14976331/use-of-tilde-in-r-programming-language
#The thing on the right of <- is a formula object. It is often used to denote a statistical model, where the thing on the left of the ~ is the response and the things on the right of the ~ are the explanatory variables. So in English you'd say something like "Species depends on Sepal Length, Sepal Width, Petal Length and Petal Width".
library("doBy")
summaryBy(Age~age_group, data=df, FUN=siterange) #Textboook says Age~age_group
#So only signed in users have ages and genders.
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group, data = df)

#create click thru rate. We don't care abotu clicks
#if there are no impressions. If there are clicks without impressions
#our assumptions about the data are wrong.
df$hasimps <- cut(df$Impressions, c(-Inf, 0, Inf))
summaryBy(Clicks~hasimps, data = df, FUN=siterange) #Summarize clicks by different hasimps categories


#create categories
df$scode[df$Impressions == 0] <- "NoImps"
df$scode[df$Impressions > 0] <- "Imps"
df$scode[df$Clicks > 0] <- "Clicks"

#Convert the column to a factor NOT SURE WHAT THIS IS DOING. Data type factor?
df$scode <- factor(df$scode)
head(df)

#look at levels. Creates a function, clen, that for argument x
#takes the length of x. Creates variable etable to save the length of
#the number of impressions summarized by scode, Gender, and age group?? 
#Counts number of impressions by  Impression/Clicks category (scode), further
#broken down by gender and age group.
clen <- function(x){c(length(x))}
paste("etable", counter, ".csv" )<- summaryBy(Impressions~scode+Gender+age_group, data = df, FUN=clen)
rm(df)
counter+1


#for(i in counter:dirlength){
#  nam <- paste("etable", counter, ".csv", sep="")
#  assign(nam,1:i)
#}
}