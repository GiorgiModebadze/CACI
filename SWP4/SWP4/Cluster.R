Clusters = read.csv("../Clusters.csv")
Clusters
dataCluser = read.csv("../cbc_data.csv")
dataCluser

dataClusered = left_join(Clusters,dataCluser ,"id")
dataClusered

dataClusered = dataClusered %>% filter(!(id %in% a$id))


dataClusered

cluster1 = filter(dataClusered, seg.hc.segment == 1)
cluster2 = filter(dataClusered, seg.hc.segment == 2)
cluster3 = filter(dataClusered, seg.hc.segment == 3)
cluster4 = filter(dataClusered, seg.hc.segment == 4)

cluster = function(x) {
  x$seg.hc.segment = NULL
  data.cbc<-x
  data.cbc$price<-data.cbc$price/100
  data_ml_bluetooth <- mlogit.data(data.cbc, choice = "choice", shape = "long",
                                   id.var = "id", alt.var = "alt")
  
  mnl_bluetooth = mlogit(choice ~ -1 +none+price+battery1+battery2+battery3+battery4+weight1+weight2+weight3+
                           sound1+sound2+sound3, data = data_ml_bluetooth)
  
  return( as.data.frame(mnl_bluetooth$coefficients))
}

clust1 = cluster(cluster1)
clust2 = cluster(cluster2)
clust3 = cluster(cluster3)
clust4 = cluster(cluster4)

clust1
