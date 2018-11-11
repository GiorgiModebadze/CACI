getwd()

library(readxl)
library(tidyverse)

attributesRating = read_excel("Data_Chocolate_allinterviews.xlsx", 
                              sheet = "AttributeRatingsStacked" ,col_names = T)

glimpse(attributesRating)
