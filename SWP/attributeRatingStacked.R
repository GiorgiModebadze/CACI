getwd()

library(readxl)
library(tidyverse)

attributesRating = read_excel("Data_Chocolate_allinterviews.xlsx", 
                              sheet = "AttributeRatingsStacked" ,col_names = T)

glimpse(attributesRating)
head(attributesRating[-1])
summary(attributesRating)

everConsumedClean

(left_join(everConsumedClean, attributesRating, by = c("N" = "Person"))) %>% 
  filter(CBar == Product) %>% select(N, Product, CBar)

select(attributesRating ,Person , Product) %>% distinct(.)

describeBy(attributesRating[-1],"Product")$Balisto


aggregate(attributesRating,by= "Product",FUN =  mean)


gatheredData = gather(attributesRating[-1],  Attribute, Vals, -Product ) 

gatheredData = na.omit(gatheredData)
gd = gatheredData %>% mutate(Name = paste(Product,Attribute, sep=":"))

gd
ggplot(gd, aes(x = Product, y = Vals)) + geom_boxplot() + coord_flip() + ylab("Perception") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend  = T)+
  facet_wrap(~Attribute)



gd %>% group_by(Product, Attribute) %>% summarise(mean = mean(Vals))%>% 
  group_by(Attribute) %>% filter(mean == max(mean))

gd %>% group_by(Product, Attribute) %>% summarise(mean = mean(Vals))%>% 
  group_by(Attribute) %>% filter(mean == min(mean))

<<<<<<< HEAD


summary(attributesRating)
sapply(attributesRating, function(x) sum(is.na(x)))

=======
>>>>>>> 56848e65b8d8850b5086f3f90071cce54288fcd1
missing = sapply(attributesRating, function(x) sum(is.na(x)))
missing = missing[-1]
missing = missing[-1]


sum(sapply(filter(attributesRating,Product =="Duplo"),function(x) sum(is.na(x))))
missing1 = c(sum(sapply(filter(attributesRating,Product =="Snickers"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="KinderBueno"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="Twix"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="Mars"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="KitKat"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="Bounty"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="KinderRiegel"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="Balisto"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="Lion"),function(x) sum(is.na(x)))),
             sum(sapply(filter(attributesRating,Product =="Duplo"),function(x) sum(is.na(x)))))
names(missing1) = c ("Snickers","KinderBueno","Twix","Mars","KitKat","Bounty","KinderRiegel","Balisto","Lion","Duplo")
missing1
barplot(missing1)
<<<<<<< HEAD




gather = gatheredData %>% filter( is.na(Vals)) %>% group_by(Product,Attribute) %>% summarize( c = sum(is.na(Vals)))

spread = spread(gather, Product, c,)

spread[is.na(spread)] <-0

gatheredNA =   gather(spread,"cat","nas", - Product )
gatheredNA

ggplot(gatheredNA, aes(x = Product, y = nas)) + geom_col() + coord_flip() + ylab (" Number of missing")
