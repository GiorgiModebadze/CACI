
getwd()
rm(list = ls())

install.packages("vegan")
library(vegan)
library(cluster)
library(readxl)
library(nFactors)
library(tidyverse)
library(GPArotation)
library(psych)

getwd()
# read Data

dat <-read_excel("/Users/Raviky/Documents/GitHub/CACI/SWP/Data_Chocolate_allinterviews.xlsx", 
                 sheet = "AttributeRatingsStacked")
dat<-as.data.frame(dat)
str(dat)
head(dat)
bars<-dat
#finding outliers
bars.dt.long %>% filter(!is.na(Value) & Product == "KitKat" & Attribute == "crunchy") %>% group_by(Product, Attribute) %>%
  summarise(mean = mean( Value) )

aggregate(dat[,-c(1,2)], by=list(bars$Product),mean, na.rm=TRUE)

library(data.table)
# use data.table
bars.dt = as.data.table(bars)
# melt data.table to "long" format
bars.dt.long = melt(bars.dt, id.vars = c("Person", "Product"), 
                    variable.name = "Attribute", value.name = "Value") 
str(bars.dt.long)
head(bars.dt.long)
# mean values for each attribute (by each product and attribute)
bars.dt.long[, Mean.by.prod.att := mean(Value, na.rm = TRUE), by = .(Product, Attribute)]
head(bars.dt.long)
# test
unique(bars.dt.long[, .(Product, Attribute, Mean.by.prod.att)])


# impute
bars.dt.long[is.na(Value), Value := Mean.by.prod.att]
bars.dt.2 = dcast(bars.dt.long, Person + Product ~ Attribute, value.var = "Value")

## FA 



# check eigenvalues

eigen(cor(bars.dt.2[,3:15]))$values
plot(eigen(cor(bars.dt.2[,3:15]))$values, ylab = "eigen",
     main = "Number of Factors")
abline( h = 1, col = "red",)

a.fa. <- fa(bars.dt.2[,3:15],fm="ml", max.iter=1000,SMC=TRUE,scores='Anderson',nfactors=5, 
          rotate ="varimax")

a.fa.


library(gplots)
library(RColorBrewer)
library(semPlot)
heatmap(a.fa.$loadings,col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none",
          Colv=FALSE,
          main="Factor loadings for brand adjectives")



brand.fa.ob <- factanal(bars.dt.2[,3:15], factors=5, rotation="oblimin")
semPaths(brand.fa.ob, what="est", residuals=FALSE,
           cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
           edge.label.cex=0.75, nCharNodes=7)


# PCA BI plot


a.pca<-principal(bars[,3:15],nfactors=2)
a.pca$values
biplot(a.pca)
plot(a.pca$values)
a.pca
str(a.pca$score)
aggregate(a.pca$scores, by=list(bars$Product),mean, na.rm=TRUE)

str(bars.dt.2)
brand.sc <- bars.dt.2
brand.mean <- aggregate(. ~ Product, data=brand.sc, mean)
brand.mean
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1] 
brand.mean
brand.mu.pc <- prcomp(brand.mean)

heatmap.2(as.matrix(brand.mean),
                   col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
                   main="\n\n\n\n\nBrand attributes")



biplot(brand.mu.pc, main="Brand positioning",
       cex = 0.5)

?biplot

