library(tidyverse)
library(MASS)


data = read.csv("../cbc_data.csv")


clean = data %>% filter(!(id %in% a$id))


cleandb = clean %>% mutate(battery = case_when(battery1 == 1 ~ "8h",
                                               battery2 == 1 ~ "10h",
                                               battery3 == 1 ~ "12h",
                                               battery4 == 1 ~ "14h",
                                               TRUE ~ "16h"),
                           weight = case_when (weight1 == 1 ~ "400g",
                                               weight2 ==1 ~ "500g",
                                               weight3 == 1 ~ "600g",
                                               TRUE ~ "700g"),
                           sound = case_when(sound1 == 1 ~ "3.5s",
                                             sound2 == 1 ~ "4.0s",
                                             sound3 == 1 ~ "4.5s",
                                             TRUE ~ "5.0s")) %>%
  dplyr::select(id, cs, alt, choice, none, price, battery, weight, sound) %>%
  mutate(battery = as.factor(battery), weight = as.factor(weight),
         sound = as.factor(sound))

str(cleandb)



str(clean)
temptb = data %>% filter(choice == 1) %>%  group_by(price) %>%
  summarise(b1 = sum(battery1),b2 = sum(battery2),b3= sum(battery3),
                                           b4= sum(battery4), s1= sum(sound1),
            s2 = sum(sound2), s3 = sum(sound3), w1 = sum(weight1), w2 = sum(weight2),
            w3 = sum(weight3))

temptb = data %>% filter(choice  ==1 ) %>% mutate(a = paste(price, battery1, battery2,
                                                            battery3, battery4, weight1,
                                                            weight2, weight3, sound1,sound2,
                                                            sound3, " ")) 
temptb %>% group_by(id, a) %>% summarise( n = n()) %>% arrange(desc(n)) %>% 
  filter(n == 12)


temptb %>% group_by(a) %>% summarise( c = n()) %>% arrange(desc(c))


## row choice counts
filter(data, choice==1) %>% count(price)
filter(data, choice==1) %>% count(battery1)
cor(data)

#From book : We encourage you to compute choice counts for each attribute before 
#estimating a choice model.

# create long version of data
head(data,10)

a = data %>% filter(choice == 1) %>% group_by(id, alt) %>% summarise(n = n()) %>% 
  count(id) %>% arrange(nn)  %>% filter(nn == 1)




# battery1 	8 hours
# battery2	10 hours
# battery3	12 hours
# battery4    	14 hours
# 16 hours  (omitted level)
# 
# weight1		400 grams
# weight2		500 grams
# weight3		600 grams
# 700 grams (omitted level)
# 
# sound1		3.5 stars
# sound2		4.0 stars
# sound3		4.5 stars
# 5.0 stars (omitted level)


cleanWithout0 = cleandb %>% mutate(battery = ifelse(alt ==4 , 
                                          0, battery ),
                                   weight =  ifelse(alt == 4,
                                                    0, weight ),
                                   sound =  ifelse(alt == 4,
                                                   0, sound )) %>%
  mutate(battery = as.factor(battery), weight = as.factor(weight),
         sound = as.factor(sound))
  


cleanData = cleandb      
library(mlr)
cleanData = mlr::createDummyFeatures(cleanData)
cleanData

cleanData = cleanData %>% mutate(battery.16h = ifelse(alt ==4 , 
                                                    0, battery.16h ),
                                 weight.700g =  ifelse(alt == 4,
                                                    0, weight.700g ),
                                 sound.5.0s =  ifelse(alt == 4,
                                                   0, sound.5.0s ))


data
