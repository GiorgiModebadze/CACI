library(datasets)
library(ggplot2)
library(dplyr)
eurodist

mds = cmdscale(eurodist)

names(mds)

plot(mds, type ='n')
text(mds[,1],mds[,2], labels(eurodist))
ggplot(mds, x =)

