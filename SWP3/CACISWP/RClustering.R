library(tidyverse)
library(klaR)
library(mlr)
library(cluster)
length(bluetooth)
selected = bluetooth %>% dplyr::select(13:26)
bluetooth = data
bluetooth
selected

seg.summ <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}


selectedDummy = mlr::createDummyFeatures(selected)
str(selectedDummy)
selectedDummy
bluetooth
set.seed(96743)
seg.k <- kmeans(selectedDummy, centers=3)

seg.summ(bluetooth, seg.k$cluster)

a = clusplot(bluetooth, seg.k$cluster, color=TRUE, shade=TRUE,
          lines=0, main="K-means cluster plot")

ggsave("~/Documents/GitHub/CACI/SWP3/CACISWP/clust.png", ,
       dpi = 320, height = 50, width = 100, units = "mm")


library(cluster) 

seg.dist = daisy(bluetooth)
as.matrix(seg.dist)[1:5, 1:5]

seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)

plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
bluetooth[c(478, 561), ]
bluetooth[c(478,430),]

cor(cophenetic(seg.hc), seg.dist)


plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")

seg.hc.segment <- cutree(seg.hc, k=4)     # membership vector for 4 groups
table(seg.hc.segment)
seg.hc.segment
seg.hc.segment

cbind(seg.hc.segment, data) %>% dplyr::select(seg.hc.segment, id)

  write.csv(cbind(seg.hc.segment, data) %>% dplyr::select(seg.hc.segment, id), "~/Documents/GitHub/CACI/SWP4//Clusters.csv",row.names = F,
            quote = F)

seg.summ(bluetooth, seg.hc.segment)[,24:37]
