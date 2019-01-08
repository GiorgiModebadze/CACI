
library(tidyverse)
library(psych)
library(skimr)
library(ggthemes)
data = read.csv("../indivData.csv")
data$IncomeLabel =  gsub("\x80",replacement = "",x = data$IncomeLabel)
data$IncomeLabel = ordered(data$IncomeLabel, levels = c("<500","501-1000","1001-1500",
                                                          "1501-2000","2001-2500","2501-3000",
                                                          ">=3001","rather not say"))

distinct(data,IncomeLabel)

#lets do some exploratory data anaylsis

#1 check where people are from
filter(data, Own == 0) %>%
count( IncomeLabel,IntentToBuy) 
# most of them are from germany

## lets check income distribution

IncomeByOccupation = ggplot(data, aes(x = IncomeLabel, col = OccupationLabel)) + 
  geom_bar(stat="count") +
  theme_excel_new() +
  labs(title = "Income by occupation") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 5)) +
    scale_fill_brewer(palette="Set3")  

IncomeByOccupation
ggsave("/Users/Raviky/Documents/GitHub/CACI/SWP3/CACISWP/IncomeByOccupation.png",
       IncomeByOccupation,dpi = 320, height = 50, width = 100, units = "mm")

##  how they are occupied by gender


tempOcc = count(data, OccupationLabel)
tempOccInc = count(data, IncomeLabel, OccupationLabel)

tb  = left_join(tempOccInc,tempOcc,"OccupationLabel") %>% 
  mutate(prc = paste0(round(n.x/n.y * 100), "%")) %>%
  select (IncomeLabel, OccupationLabel, prc) %>% 
  spread( key= IncomeLabel, value = prc)
tb[is.na(tb)] = "0%"
tb
grid.newpage()
grid.table(tb)
## lets check age
ggplot(data, aes(x = Age)) + geom_density() 

# all kinds of people mainly 18-24 and 25-29

## lets find occupation by age
ggplot(data, aes(x = OccupationLabel)) + geom_bar() + facet_wrap(~AgeLabel) + 
  coord_flip()

ggplot(data, aes(x = IncomeLabel)) + geom_bar() + facet_wrap(~AgeLabel) + 
  coord_flip()

## most people dont own Items
library(ggthemes)

tempf = count(data, Own, IncomeLabel, IntentToBuy)
tempf$IntentToBuy = as.factor(tempf$IntentToBuy)
tempf$Own = as.factor(tempf$Own)
tempf$IncomeLabel
levels(tempf$IntentToBuy) = c("No","Yes")
levels(tempf$Own) = c("Own - No","Own - Yes")
tempf = mutate(tempf, prc = round(prop.table(n) * 100))
str(tempf)


plt = ggplot(tempf, aes(x = IncomeLabel, y= n, fill = as.factor(IntentToBuy))) + 
  geom_col(width=.7, position = "dodge") + theme_excel_new() +
  facet_grid(. ~ Own) + coord_flip() +
  labs(title = "Own Vs Intention to buy by Income Level",
       caption = "The percentages on the bars represent share of each bar in total count") +
  scale_fill_manual(values=c( "#E69F00","#56B4E9"),
                    name="Intent To Buy",
                    breaks=c("No", "Yes"),
                    labels=c("No", "Yes")) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10)) +
  geom_text(aes(y = n/2,    # nudge above top of bar
                label = paste0(prc, '%')),    # prettify
            position = position_dodge(width = .5), 
            size = 2.5)

plt
       
ggsave("/Users/Raviky/Documents/GitHub/CACI/SWP3/CACISWP/Three.png",plt,dpi = 320, height = 150, units = "mm")

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
a
a = select(data, starts_with("BrandAwareness_"))
colnames(a) = sub("BrandAwareness_", "", colnames(a))
corBrand =cor(a)
dist = dist(corBrand)
scale = cmdscale(as.matrix(dist),k=2)
colnames(scale) = c("X","Y")
scale

plot(scale, main = "brand awarness", type = "n")
text(scale, labels = rownames(scale), cex = 0.5)

## to get information about how people understand brand Awareness
install.packages("wesanderson")
library(wesanderson)
tempb = select(data, IncomeLabel,OccupationLabel, starts_with("BrandAwareness_")) %>%
  gather("Manufacturer",value = "Know",3:11) %>% 
  mutate(Manufacturer = gsub(pattern = "BrandAwareness_", "", Manufacturer) ) %>%
  filter(OccupationLabel != "Retired") 

tempTotal  = tempb %>% summarise(sum(Know))
tempTotal
prc = tempb %>% group_by(Manufacturer) %>% summarise(prc = sum(Know)/tempTotal$`sum(Know)` * 100)

library(plotly)

plot_ly(tempb, labels = ~Manufacturer, values = ~Know, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(showlegend = FALSE,
         title = 'Brand Awareness',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

brandAwarenessByOccplt = ggplot(tempb, aes(x = Manufacturer,  y = Know, fill = Manufacturer)) + geom_bar(stat="identity") +
  facet_wrap(~OccupationLabel) + theme_excel_new() + coord_flip() +
  scale_fill_brewer(palette="Set1") +
  labs(title = "Brand Awareness By Occupational Status") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10))

ggsave("/Users/Raviky/Documents/GitHub/CACI/SWP3/CACISWP/brandAwarenessByOccplt.png",
       brandAwarenessByOccplt,dpi = 320, height = 150, units = "mm")



# ggplot(tempb, aes(x = "a" ,y = Know, fill= factor(Manufacturer))) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start=0) + 
#   labs(x = NULL, y = NULL, fill = NULL, title = "By Brand") +
#   theme_classic()  +
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "#666666")) +
#   scale_fill_brewer(palette="Set1") 

## people who intnt to buy stuff. Their Preferences.
library(gridExtra)

relImportance = select(data,IntentToBuy, OccupationLabel, starts_with("RelImp_"))%>%
  group_by(IntentToBuy, OccupationLabel)  %>% filter (OccupationLabel != "Retired") %>%
  summarise(TotalRespodents = n(),
            Battery = round(mean(RelImp_battery)),
            Price = round(mean(RelImp_price)),
            Sound = round(mean(RelImp_sound)),
            Weight = round(mean(RelImp_weight))) %>%
  gather("Attribute", "Value", 4:7)

relImportance$IntentToBuy = as.factor(relImportance$IntentToBuy)
levels(relImportance$IntentToBuy) = c("Intent to Buy - No", "Intent to Buy - Yes")
relImportance

plt2 = ggplot(relImportance, aes(x = OccupationLabel, y = Value, fill = factor(Attribute))) + 
  geom_col(position = "dodge") +   theme_excel_new() +
  facet_grid(IntentToBuy ~. ) +
  geom_text(aes(y = Value/2,    # nudge above top of bar
                    label = paste0(Value, '%')),    # prettify
                    position = position_dodge(width = .9), 
                    size = 2.5) +
  labs(title = "Relative Importance of Features",
       subtitle = "By Ocupational Status and Intention to buy new product"
       ) + 
  scale_fill_manual(values=c( "#E69F00","#56B4E9", "#00b37c","#9f00e6"),
                    name="Attribute",
                    breaks=c("Battery", "Price", "Sound", "Weight"),
                    labels=c("Battery", "Price", "Sound", "Weight")) +
  theme(plot.subtitle = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10))

ggsave("/Users/Raviky/Documents/GitHub/CACI/SWP3/CACISWP/two.png",plt2,dpi = 320, height = 150, units = "mm")

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

tempFactAnal = factanal(SubjKnowledge, factors =2, rotation="oblimin")
semPaths(tempFactAnal, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)
tempFactAnal
# Using factor Analysis we can see that Two factors are sufficient to explain
# 72% of the variation, Intrestingly enough Question number three was only question
# that differed from others 
boxplot(SubjKnowledge)

# using boxplot we can clearly see that people are less confident when they considering
# themselves as experts between experts 

require(corrplot)
corrplot(cor(SubjKnowledge))

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
SubjKnowledgeByGender = ggplot(SubjKnowledge, aes(x =GenderLabel, y = average )) + geom_boxplot() + 
  geom_abline(intercept = median(SubjKnowledge$average), slope = 0, colour = "red",
              linetype = "dashed", size = 1) + theme_excel_new()+
  labs(title = "Subject Knowledge by Gender") + 
  theme(plot.subtitle = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10))
 
ggsave("/Users/Raviky/Documents/GitHub/CACI/SWP3/CACISWP/SubjKnowledgeByGender.png",
       SubjKnowledgeByGender,dpi = 320, height = 150, units = "mm")

# we can argue that males know more about speakers than females

