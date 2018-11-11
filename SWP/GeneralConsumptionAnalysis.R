getwd()
setwd("SWP/")

#load libraries
require(readxl)
require(tidyverse)

#load consuption data
consumptionData = read_excel(path = "Data_Chocolate_allinterviews.xlsx", 
                             sheet = "Gerneral Consumption", col_names = T)

summary(consumptionData)

#assign new more managable names to the data
consumptionData = setNames(object = consumptionData,
                           nm = c("N", "Frequency",  "Place", "Reason",
                                  "everConsumed"))

consumptionData

#create separate table for everConsumed data that later can be joined with
#consumptionData
tb = separate_rows(consumptionData, everConsumed, sep = "," )

everConsumedClean = tibble(N = tb$N, CBar = trimws(tb$everConsumed))

everConsumedClean

#plot which bars were consumed by which user
ggplot(everConsumedClean, aes(x = CBar)) + geom_bar()

#create separate table for Place data 
tb = separate_rows(consumptionData, Place, sep = "," )

placeClean = tibble(N = tb$N, Place = trimws(tb$Place))

placeClean

# plot by place 
ggplot(placeClean, aes(x = Place)) + geom_bar()


       