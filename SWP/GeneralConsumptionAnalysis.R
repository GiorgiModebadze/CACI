getwd()
setwd("SWP/")


# load libraries
require(readxl)
require(tidyverse)
require(data.table)

# load consuption data

#load libraries
require(readxl)
require(tidyverse)

#load consuption data
consumptionData = read_excel(path = "Data_Chocolate_allinterviews.xlsx", 
                             sheet = "Gerneral Consumption", col_names = T)

summary(consumptionData)

<<<<<<< HEAD
# assign new more managable names to the data
=======
#assign new more managable names to the data
>>>>>>> a79a3fbb288c3cebdbfaa8113da9483b52c33655
consumptionData = setNames(object = consumptionData,
                           nm = c("N", "Frequency",  "Place", "Reason",
                                  "everConsumed"))

consumptionData


# create separate table for everConsumed data that later can be joined with
# consumptionData

#create separate table for everConsumed data that later can be joined with
#consumptionData

tb = separate_rows(consumptionData, everConsumed, sep = "," )

everConsumedClean = tibble(N = tb$N, CBar = trimws(tb$everConsumed))

everConsumedClean


# plot which bars were consumed by which user
ggplot(everConsumedClean, aes(x = CBar)) + geom_bar() + coord_flip()

# create separate table for Place data 

#plot which bars were consumed by which user
ggplot(everConsumedClean, aes(x = CBar)) + geom_bar()

#create separate table for Place data 

tb = separate_rows(consumptionData, Place, sep = "," )

placeClean = tibble(N = tb$N, Place = trimws(tb$Place))

placeClean

# plot by place 
ggplot(placeClean, aes(x = Place)) + geom_bar()

# plot by frequency 
frequencyClean = tibble(N = consumptionData$N, 
                           Frequency = consumptionData$Frequency)

ggplot(frequencyClean, aes( x =Frequency)) + geom_bar()

table(frequencyClean$Frequency)

# as it seems there is a person who said never to the consumption. it would be
# smart to exclude this person from further analysis or at least check his other
# answers

filter(consumptionData, Frequency == "never") # person number 43

# check and analyze reason 

table(consumptionData$Reason) 
# all the rows differ a lot. lets separate them by commas and try to make sense
# out if it

tb = separate_rows(consumptionData, Reason, sep = ",")

reasonClean = tibble(N =tb$N, Reason = trimws(tb$Reason))

reasonClean = as.data.table(reasonClean)

# clean data a bit more 
reasonClean[ , Reason := .(gsub("when I am ","",Reason)) ]
reasonClean[ , Reason := .(gsub("while ","",Reason)) ]
reasonClean[ , Reason := .(gsub("when ","",Reason)) ]
reasonClean[ , Reason := .(gsub("as a ","",Reason)) ]
reasonClean[ , Reason := .(gsub("my","",Reason)) ]

table(reasonClean$Reason)

## clean when I feel like it and  no particular circumstance given as same 
filter(reasonClean, Reason == "when I feel like it" |
         Reason == "no particular circumstance given" |
         Reason == "and when i feel like it") 

reasonClean[ , Reason := .(gsub("no particular circumstance given","No Reason",Reason))]
reasonClean[ , Reason := .(gsub("and i feel like it","No Reason",Reason)) ]
reasonClean[ , Reason := .(gsub("I feel like it","No Reason",Reason)) ]

# N 19 and N 30 are the one with that reasons. Replace them with No Reasons

reasonClean = as_tibble(reasonClean)
table(reasonClean$Reason)

ggplot(reasonClean , aes(x= Reason)) + geom_bar() + coord_flip() 

# join all cleaned tables to get final clean table 

cleanJoinedConsumption = left_join(everConsumedClean, frequencyClean, by= "N") %>%
  left_join(.,placeClean, by = "N") %>% left_join(., reasonClean, by="N")

cleanJoinedConsumption

# remove tb as it was temporary 
rm(tb)

# now we can analyze clean data

ggplot(reasonClean , aes(x= Reason)) + geom_bar() + coord_flip() 
ggplot(everConsumedClean, aes(x = CBar)) + geom_bar() + coord_flip()
ggplot(placeClean, aes(x = Place)) + geom_bar()
ggplot(frequencyClean, aes( x =Frequency)) + geom_bar()

