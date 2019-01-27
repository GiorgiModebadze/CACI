setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/CACI/WS1819/Data")
#install.packages('mlogit')
library(mlogit)
library(data.table)

data.cbc<-cleanData
head(cleanWithout0)
data.cbc$price<-data.cbc$price/100
head(data.cbc,20)
str(data.cbc)
data_ml_bluetooth <- mlogit.data(data.cbc, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

colnames(data.cbc)
mnl_bluetooth = mlogit(choice ~ -1 + none + price + battery + sound + weight, data = data_ml_bluetooth)

mnl_bluetooth = mlogit(choice ~ -1 + none + price + 
                        battery.10h + battery.12h + battery.14h+ battery.16h+ battery.8h+ weight.400g +
                       weight.500g + weight.600g + weight.700g + sound.3.5s + sound.4.0s +  sound.4.5s + 
                        sound.5.0s , data = data_ml_bluetooth)



summary(mnl_bluetooth)

head(data_ml_bluetooth)

ab = as.data.frame(mnl_bluetooth$coefficients)

predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}

mnl_bluetooth$coefficients

data %>% filter(id == 134)



