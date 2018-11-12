setwd("/Users/Raviky/OneDrive - hu-berlin.de/Git/RWorks/CACI/SWP/")
rm(list=ls())
library(data.table)
library(readxl)
library(data.table)
library(ggplot2)
library(psych)
library(dplyr)

attributesRatingStacked = read_excel(path = "Data_Chocolate_allinterviews.xlsx", sheet = 2 )
str(attributesRatingStacked)
class(attributesRatingStacked)

attributesRatingStackedTable = data.table(attributesRatingStacked)
describeBy(attributesRatingStackedTable, "Product")


## Analyze Audience and how representativ they are

socialDemographics = read_excel(path = "Data_Chocolate_allinterviews.xlsx", 
                                     sheet = "Social Demographic Questions" )


socialDemographicstb = as.data.table(socialDemographics)
socialDemographicstb

names(socialDemographicstb) 
setnames(socialDemographicstb,names(socialDemographicstb),
         c("Person","Gender","Age","Occupation","mStatus","nKids","LivingPlace","State","Sport"))

head(socialDemographicstb)
str(socialDemographicstb)

ggplot(socialDemographicstb, aes(x= mStatus, fill = Gender)) + geom_histogram(stat= "count") 



#correct data in States table
#occupation
socialDemographicstb[ , Occupation := .(gsub("I am ","",Occupation)) ]
socialDemographicstb[ , Occupation := .(sub("an","",Occupation)) ]
socialDemographicstb[ , Occupation := .(gsub("a ","",Occupation)) ]
socialDemographicstb[ , Occupation := .(sub("\\.","",Occupation)) ]
socialDemographicstb[ , Occupation := .(sub("At home for Babys","Not Working",Occupation)) ]

#state
socialDemographicstb[ , State := .(gsub("berlin","Berlin",State))]
socialDemographicstb[ , State := .(gsub("sachsen-anhalt","Sachsen-Anhalt",State))]
socialDemographicstb[ , State := .(gsub("NRW","Nordrhein-Westfalen",State))]
socialDemographicstb[ , State := .(gsub("North Rhine Westfalia","Nordrhein-Westfalen",State))]
socialDemographicstb[ , State := .(gsub("North-Rhine-Westpfalia","Nordrhein-Westfalen",State))]
socialDemographicstb[ , State := .(gsub("North-Rhine-Westpfalia","Nordrhein-Westfalen",State))]
socialDemographicstb[ , State := .(gsub("Saxony","Sachsen",State))]
socialDemographicstb[ , State := .(gsub("Sachsen Anhalt","Sachsen-Anhalt",State))]
socialDemographicstb[ , State := .(gsub("Sachsen-Anhalt-Anhalt","Sachsen-Anhalt",State))]
socialDemographicstb[ , State := .(gsub("Sachsen-Anhalt Anhalt","Sachsen-Anhalt",State))]
socialDemographicstb[ , State := .(gsub("saxony anhalt","Sachsen-Anhalt",State))]
socialDemographicstb[ , State := .(gsub("SA","Sachsen-Anhalt",State))]
socialDemographicstb[ , State := .(gsub("Hesse","Hessen",State))]
socialDemographicstb[ , State := .(gsub("Hessenn","Hessen",State))]
socialDemographicstb[ , State := .(gsub("Hessennn","Hessen",State))]
socialDemographicstb[ , State := .(gsub("Bavaria","Bayern",State))]
socialDemographicstb[ , State := .(gsub("Munich","Bayern",State))]
socialDemographicstb[ , State := .(gsub("Leipzig","Sachsen",State))]
socialDemographicstb[ , State := .(gsub("Hannover","Niedersachsen",State))]

socialDemographicstb[,.N, by = .(State, Gender, nKids)][order(State,nKids)]

##
socialDemographicstb

ggplot(socialDemographicstb, aes(x = Occupation, fill = Sport)) + geom_bar()

ggplot(socialDemographicstb, aes(x = State, fill =Gender )) + geom_bar()
summary(socialDemographicstb)
boxplot(socialDemographicstb$Age)


table(socialDemographicstb$State)

