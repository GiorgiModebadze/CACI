library(tidyverse)
library(psych)
library(skimr)
data = read.csv("../indivData.csv")

skim(data)
head(data)

#lets do some exploratory data anaylsis

#1 check where people are from
filter(data, Own == 0) %>%
count( IncomeLabel,IntentToBuy) 
# most of them are from germany

## lets check income distribution

ggplot(data, aes(x = Income)) + geom_density()

##  how they are occupied by gender
count(data, OccupationLabel, GenderLabel)

## lets check age
ggplot(data, aes(x = Age)) + geom_density()

# all kinds of people mainly 18-24 and 25-29

## lets find occupation by age
ggplot(data, aes(x = OccupationLabel)) + geom_bar() + facet_wrap(~AgeLabel) + 
  coord_flip()

ggplot(data, aes(x = IncomeLabel)) + geom_bar() + facet_wrap(~AgeLabel) + 
  coord_flip()

## most people dont own Items
count(data, Own, GenderLabel, IntentToBuy)

## mainly males own speakers, but also mostly males were questioned.
count(data, Own, IntentToBuy)
skim(data)
select(data,Own, starts_with("RelImp_"))%>% group_by(Own) %>% 
  summarise(Battery = mean(RelImp_battery),
            Price = mean(RelImp_price),
            Sound = mean(RelImp_sound),
            Weight = mean(RelImp_weight))
## For people sound is most important thing, Weight least important
apply(ImportanceMatrix,2,mean)

skim(data)

## check by occupation intent to buy

ggplot(data, aes(x = OccupationLabel, y = IntentToBuy)) + geom_col()

## check brand avearness

a = select(data, starts_with("BrandAwareness_"))
colnames(a) = sub("BrandAwareness_", "", colnames(a))
corBrand =cor(a)
dist = dist(corBrand)
scale = cmdscale(as.matrix(dist),k=2)
colnames(scale) = c("X","Y")

plot(scale, xlim = c(-2 ,2), main = "brand awarness", type = "n")
text(scale, labels = rownames(scale), cex = 0.5)

## to get information about how people understand brand Awareness

skim(data)


## people who intnt to buy stuff. Their Preferences.
library(gridExtra)
grid.table(
select(data,Own,IntentToBuy, OccupationLabel, starts_with("RelImp_"))%>%
  group_by(Own,IntentToBuy, OccupationLabel) %>% filter(IntentToBuy == 1) %>%
  summarise(TotalRespodents = n(),
            Battery = round(mean(RelImp_battery),2),
            Price = round(mean(RelImp_price),2),
            Sound = round(mean(RelImp_sound),2),
            Weight = round(mean(RelImp_weight),2))
)


## lets check brand avareness between people
select(data, IntentToBuy,OccupationLabel, starts_with("BrandAwareness_")) %>%
  group_by(IntentToBuy,OccupationLabel ) %>% summarise_all(.funs = sum)  %>%
  rename_at(vars(starts_with("BrandAwareness_")), funs(sub("BrandAwareness_", "", .)))


library(colSums)

## we can check peoples knowledge of brands and their intention to buy. if there is
# a connection between this two factors we can draw some conclusions
select(data,  starts_with("BrandAwareness_"), - starts_with("BrandAwareness_None")) %>% rowSums(.) %>%
  as.tibble(.) %>% ggplot(aes(x = value )) +   geom_density(bw = 1)

## intent to buy and knowledge of brands
tempdata =select(data, IntentToBuy, starts_with("BrandAwareness_"), - starts_with("BrandAwareness_None")) %>%
  mutate(IntentToBuy = ifelse(IntentToBuy==1, "Yes", "No")) 
a = tempdata[,-1] %>% rowSums(.) %>% as.tibble(.) 
as.tibble(cbind(a$value, tempdata$IntentToBuy)) %>% mutate(V1 = as.integer(V1)) %>%
  ggplot(aes(x = V1)) +   geom_density(bw = 1) + facet_wrap(~V2)

count (data,BrandAwareness_None, IntentToBuy)
count (data,BrandAwareness_None)

count(data, AgeLabel, Own,IntentToBuy)
count(data, IncomeLabel, IntentToBuy)
count(data, Own, IntentToBuy)

# analyzing PII

PII = select(data,starts_with("PII"))

head(PII)
PIIRange
PIIRange = cbind(PII, as.tibble(apply(PII,1, function(x) max(x)-min(x))))
PIIRange = cbind(PIIRange, as.tibble(apply(PII,1, mean)))
rownames(PIIRange) = data$id

PIIRange[order(-PIIRange$value),]


# analyze subject knowledge
SubjKnowledge = select(data ,starts_with("Subj"))

# as study suggests : https://www.sciencedirect.com/science/article/pii/S0148296398000575 
# we should run PCA and FA first to determine coefficient alpha and a matrix of Pearson correlations

#quote from text 
# Item responses were factor analyzed using principal axis factoring. Coefficient 
# alpha and a matrix of Pearson correlations were computed. Coefficient alpha for 
# the nine items was 0.91. Only one item, number 9, reduced coefficient alpha. At
# the same time, the factor analysis produced a solution with two factors that 
# explained 64.1% of the variance. Items 1, 4, 5, 6, 7, and 8 formed the first 
# factor and items 2, 3, and 9 formed the second. The two factors were correlated 
# at 0.64. Again, item 9 was the weakest, with communality of 0.32.

PCASurvey = prcomp(SubjKnowledge)

scree(SubjKnowledge)
plot(PCASurvey)

# if we take a look we can say that one factor is enough for explaining all the 
# knowledge data. meaning users were quite consistant in aswering questions
cor(SubjKnowledge)
factanal(SubjKnowledge, factors =2, rotation = "none")

require(corrplot)

rownames(SubjKnowledge) = data$id 



SubjKnowledge = cbind(SubjKnowledge,as.tibble(apply(SubjKnowledge,1, function(x) max(x)-min(x))),
                      as.tibble(apply(SubjKnowledge,1, mean)))


SubjKnowledge$GenderLabel = data$GenderLabel
SubjKnowledge$id = data$id
SubjKnowledge
colnames(SubjKnowledge)[6] = "range"
colnames(SubjKnowledge)[7] = "average"

skim(SubjKnowledge)


# check  distribution of average by sex
ggplot(SubjKnowledge, aes(x =GenderLabel, y = average )) + geom_boxplot() + 
  geom_abline(intercept = median(SubjKnowledge$average), slope = 0, colour = "red",
              linetype = "dashed", size = 1)
# we can argue that males know more about speakers than females

