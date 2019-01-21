library(tidyverse)
library(MASS)

?TeachingDemos
data = read.csv("../cbc_data.csv")
str(data)

xtabs(choice ~ price, data = data)

## row choice counts
filter(data, choice==1) %>% count(price)
filter(data, choice==1) %>% count(battery1)
cor(data)

#From book : We encourage you to compute choice counts for each attribute before 
#estimating a choice model.

# create long version of data
head(data,10)

data %>% filter(choice == 1)
