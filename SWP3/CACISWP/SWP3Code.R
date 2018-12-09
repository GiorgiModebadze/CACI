library(tidyverse)
library(psych)
library(skimr)
data = read.csv("../indivData.csv")

skim(data)
head(data)

#lets do some exploratory data anaylsis

#1 check where people are from

count(data, Residence) %>% arrange(desc(n))

# most of them are from germany

## lets check income distribution

ggplot(data, aes(x = Income)) + geom_density()

##  how they are occupied by gender
count(data, OccupationLabel, GenderLabel)

## lets check age
ggplot(data, aes(x = Age)) + geom_density()

# all kinds of people mainly 18-24 and 25-29

## lets find occupation by age
ggplot(data, aes(x = OccupationLabel)) + geom_bar() + facet_wrap(~Age) + 
  coord_flip()

## most people dont own Items
count(data, Own, GenderLabel)

## mainly males own speakers, but also mostly males were questioned.
count(data, Own, IntentToBuy)

