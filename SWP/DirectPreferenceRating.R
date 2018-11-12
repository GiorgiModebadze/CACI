getwd()
setwd("/Users/Raviky/Documents/GitHub/CACI/SWP/")


#import data
directPreferanceData = read_excel("Data_Chocolate_allinterviews.xlsx",
                                  sheet ="Direct Preference Rating", col_names =  T)



#check Average rating for each brand 

library(tidyverse)
library(readxl)
library(psych)

names(directPreferanceData)

directPreferanceData = setNames(object = directPreferanceData,nm =  c("Person", 
                                                                      "Snickers", "KinderBueno", "Twix", "Mars",
                                                                      "KitKat","Bounty","Kinderriegel","BalistoKornMix",
                                                                      "Lion","Duplo"))

b = stack(directPreferanceData[-1])
means <- aggregate(values ~ ind , b, mean)


ggplot(stack(directPreferanceData[-1]) ,aes(x = ind ,y = values)) + geom_boxplot()+ 
  coord_flip() + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend  = FALSE) +
  geom_text(data = means, aes(label = values, y = values - 0.3)) 
