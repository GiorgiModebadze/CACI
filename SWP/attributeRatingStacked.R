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


summary(attributesRating)
sapply(attributesRating, function(x) sum(is.na(x)))
