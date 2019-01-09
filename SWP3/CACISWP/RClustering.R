library(tidyverse)
library(klaR)
library(mlr)
library(cluster)
length(bluetooth)
selected = bluetooth %>% select(13:26)
bluetooth
selected

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
