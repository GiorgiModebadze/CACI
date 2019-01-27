setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/CACI/WS1819/Data")
#install.packages('mlogit')
library(mlogit)
library(data.table)
data.c
data.cbc<-cleandb
data.cbc$price<-data.cbc$price/100
head(data.cbc,8)
str(data.cbc)
data_ml_bluetooth <- mlogit.data(data.cbc, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price + battery + sound + weight, data = data_ml_bluetooth)
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


# battery1 	8 hours
# battery2	10 hours
# battery3	12 hours
# battery4    	14 hours
# 16 hours  (omitted level)
# 
# weight1		400 grams
# weight2		500 grams
# weight3		600 grams
# 700 grams (omitted level)
# 
# sound1		3.5 stars
# sound2		4.0 stars
# sound3		4.5 stars
# 5.0 stars (omitted level)


