setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/CACI/WS1819/Data")
#install.packages('mlogit')
library(mlogit)
library(data.table)

data.cbc<-read.csv("../cbc_data.csv")
data.cbc$price<-data.cbc$price/100
head(data.cbc,8)
str(data.cbc)
data_ml_bluetooth <- mlogit.data(data.cbc, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 +none+price+battery1+battery2+battery3+battery4+weight1+weight2+weight3+
                         sound1+sound2+sound3, data = data_ml_bluetooth)
summary(mnl_bluetooth)

head(data_ml_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}



new.data<-read.csv("newdata.csv")
new.data$price<-new.data$price/100
new.data<-new.data[,-(1:4)]
head(new.data,12)

predict.mnl(mnl_bluetooth,new.data)
new.data[,2]<-4
new.data[4,2]<-0
new.data
predict.mnl(mnl_bluetooth,new.data)


