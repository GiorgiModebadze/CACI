require(readxl)
require(tidyverse)

#load preference data
preferenceData = read_excel(path = "Data_Chocolate_allinterviews.xlsx", 
                             sheet = "Direct Preference Rating")
summary (preferenceData)
str(preferenceData)
bla = describe(preferenceData)
means = bla$mean
names(means)=c("People", "Snickers","KinderBueno","Twix", "Mars", "KitKat", "Bounty", "Kinderriegel","Balisto", "Lion","Duplo")
means= means[-1]
summary (means)
barplot(means, main="Average Preference Rating per product", 
        xlab="CBars")

#mean per Person (to check if there are people who give better ratings easily)
summary(preferenceData)
preferenceData$Person = NULL
meanperperson= rowMeans(preferenceData)
summary (meanperperson)
boxplot(meanperperson, main = "Mean per participant")
