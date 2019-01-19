setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/CACI/WS1819/Data")
d<-read.csv("conjointdata_swp4_rating_dummy.csv")
d$EUR200<-ifelse(d$Price==200,1,0)
d$EUR250<-ifelse(d$Price==250,1,0)
d$EUR300<-ifelse(d$Price==300,1,0)
str(d)
d$id <- as.factor(d$Lfd)

d$price <- d$Price/100

t1<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+Price+SDCard+White+Pink+Gold, data=d)
summary(t1)

t1<-lm(Rating ~ id+SCS_52 + SCS_60 + SoftwareButton+price:id+SDCard+White+Pink:id+Gold, data=d)
summary(t1)


t2<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+EUR200+EUR250+SDCard+White+Pink+Gold, data=d)
summary(t2)
t3<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+EUR250+EUR300+SDCard+White+Pink+Gold, data=d)
summary(t3)


# Effect Coding

head(d)
d$t <- d$SCS_52+d$SCS_60
d$SCS_52.effect <- ifelse(d$t==0,-1,d$SCS_52)
d$SCS_60.effect <- ifelse(d$t==0,-1,d$SCS_60)

d$SoftwareButton.effect <- ifelse(d$SoftwareButton==0,-1,d$SoftwareButton)
d$SDCard.effect <- ifelse(d$SDCard==0,-1,d$SDCard)

d$t <- d$White+d$Pink+d$Gold
d$White.effect <- ifelse(d$t==0,-1,d$White)
d$Pink.effect <- ifelse(d$t==0,-1,d$Pink)
d$Gold.effect <- ifelse(d$t==0,-1,d$Gold)
head(d)

t1<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+Price+SDCard+White+Pink+Gold, data=d)
summary(t1)
t1.e<-lm(Rating ~ SCS_52.effect + SCS_60.effect + SoftwareButton.effect+Price+SDCard.effect+White.effect+Pink.effect+Gold.effect, data=d)
summary(t1.e)






# Hierarchical model
library(Matrix)
install.packages("lme4")
library(lme4)

?lmer

d$price<-d$Price/100

t.hlm.1 <- lmer ( Rating ~ SCS_52 + SCS_60 + SoftwareButton+price+SDCard+White+Pink+Gold+
                    ( price | Lfd ),  data = d ,
                  control = lmerControl ( optCtrl = list ( maxfun =500000) ))
summary(t.hlm.1)
head(d)
individual_beta<-coef(t.hlm.1)$Lfd
str(individual_beta)
hist(individual_beta[,5],breaks = 30)
individual_beta

t.hlm.2 <- lmer ( Rating ~ SCS_52 + SCS_60 + SoftwareButton+price+SDCard+White+Pink+Gold+
                    (    1+SCS_52 + SCS_60 + SoftwareButton+price+SDCard+White+Pink+Gold | Lfd ),  data = d ,
                  control = lmerControl ( optCtrl = list ( maxfun =500000) ))

summary(t.hlm.2)

str(t.hlm.2)
individual_beta<-coef(t.hlm.2)$Lfd
individual_beta
str(individual_beta)
hist(individual_beta[,5],breaks = 30)
?lmer
apply(individual_beta,2,sd)
individual_beta


head(ranef(t.hlm.2)$Lfd)
head(coef(t.hlm.2)$Lfd,44)
head(fixef(t.hlm.2))
head(fixef(t.hlm.2)+ranef(t.hlm.2)$Lfd,6)

t<-t(as.matrix(fixef(t.hlm.2))%*%matrix(1,1,44))
t+ranef(t.hlm.2)$Lfd


head(coef(t.hlm.2)$Lfd[2,])
head(fixef(t.hlm.2)+ranef(t.hlm.2)$Lfd[43,])


head(individual_beta)
t<-cbind(ride.lmi[,2:9],individual_beta)
t<-cbind(individual_beta2,individual_beta)
cor(t)




regby <- function(x) {
  reg = lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+Price+SDCard+White+Pink+Gold, data = x)
  c(Lfd = x$Lfd[1], coef(reg), r.squared = summary(reg)$r.squared)
}
res_raw <- by(d, d$Lfd, regby)
str(res_raw)
res_raw

res_raw <- by(d, d$Lfd, regby)
tt <- do.call(rbind, res_raw)
tt
hist(tt[,7],breaks = 30)




# CBC homogeneous preferences
setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/CACI/WS1819/Data")
install.packages('mlogit')
library(mlogit)
library(data.table)

data.cbc<-read.csv("conjointdata_swp4_cbc_dummy.csv")
data.cbc$Price<-data.cbc$Price/100
head(data.cbc)
str(data.cbc)
data_ml_phone <- mlogit.data(data.cbc, choice = "choice", shape = "long",
                             id.var = "Lfd", alt.var = "Product")

mnl_phone = mlogit(choice ~ -1 +NONE+SCS_52+SCS_60+SoftwareButton+SDCard+White+Pink+Gold+Price, 
                   data = data_ml_phone)
summary(mnl_phone)
