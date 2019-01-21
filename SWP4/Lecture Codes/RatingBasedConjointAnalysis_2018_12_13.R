conjoint.df <- read.csv("http://goo.gl/G8knGV")
str(conjoint.df)
conjoint.df$speed <- factor(conjoint.df$speed )
# why ?
conjoint.df$ height <- factor(conjoint.df$height)
# why ?
summary(conjoint.df)
ride.lm <- lm(rating~speed+height+const+theme,data = conjoint.df)
summary(ride.lm)
str(conjoint.df)


conjoint.df$speed40 <- ifelse(conjoint.df$speed==40,1,0)
conjoint.df$speed50 <- ifelse(conjoint.df$speed==50,1,0)
conjoint.df$speed60 <- ifelse(conjoint.df$speed==60,1,0)
conjoint.df$speed70 <- ifelse(conjoint.df$speed==70,1,0)

ride.lm1 <- lm(rating~speed40+speed60+speed70+height+const+theme,data = conjoint.df)
summary(ride.lm1)

# ad hoc way to caclculate part-worths'
head(ride.lm$coefficients,10)
str(ride.lm)
b2<-ride.lm$coefficients[2]
b3<-ride.lm$coefficients[3]
b4<-ride.lm$coefficients[4]

a1<- -(b2+b3+b4)/4
a2<- b2-a1
a3<- b3-a1
a4<- b4-a1



# regression for each respondent
regby <- function(x) {
  reg = lm(rating ~ speed + height + const + theme, data = x)
  c(resp.id = x$resp.id[1], coef(reg), r.squared = summary(reg)$r.squared)
}

?rbind
# one regression for each resp.id
res_raw <- by(conjoint.df, conjoint.df$resp.id, regby)
ride.lmi <- do.call(rbind, res_raw)
res_raw
head(ride.lmi,20)

ride.lmi[,10]

# calculate part worths
speed<-cbind(-rowSums(ride.lmi[,3:5])/4, ride.lmi[,3:5]-rowSums(ride.lmi[,3:5])/4)
head(speed)
height<-cbind(-rowSums(ride.lmi[,6:7])/3,ride.lmi[,6:7]-rowSums(ride.lmi[,6:7])/3)
head(height)
const<-cbind(-ride.lmi[,8]/2,ride.lmi[,8]-ride.lmi[,8]/2)
head(const)
theme<-cbind(-ride.lmi[,9]/2,ride.lmi[,9]-ride.lmi[,9]/2)
head(theme)

# market simulation for 5 hypothetical products
u1 <- speed[,1]+height[,2]+theme[,1]+const[,2]
u2 <- speed[,2]+height[,2]+theme[,1]+const[,2]
u3 <- speed[,1]+height[,1]+theme[,2]+const[,2]
u4 <- speed[,3]+height[,3]+theme[,1]+const[,2]
u5 <- speed[,4]+height[,1]+theme[,1]+const[,2]
uall<-cbind(u1,u2,u3,u4,u5)
head(uall)
firstchoice <- apply(uall,1,which.max)
head(firstchoice)
table(firstchoice)
table(firstchoice)/length(firstchoice)*100

uexpall<-(exp(uall))
usumexp<-apply(uexpall, 1, sum)
prob <- uexpall/usumexp*100
head(prob)
?rowsum
head(uexpall)


# Hierarchical model
library(Matrix)
install.packages("lme4")
library(lme4)

?lmer

ride.hlm.1 <- lmer ( rating ~ 1+speed + height + const + theme +
                        ( 1+speed + height + const + theme || resp.id
                        ),  data = conjoint.df ,
                      control = lmerControl ( optCtrl = list ( maxfun =500000) ))
summary(ride.hlm.1)
summary(ride.hlm2a)
str(ride.hlm2)
individual_beta<-coef(ride.hlm2)$resp.id
hist(individual_beta[,5],breaks = 30)
?lmer
apply(individual_beta,2,sd)
individual_beta

head(individual_beta)
t<-cbind(ride.lmi[,2:9],individual_beta)
t<-cbind(individual_beta2,individual_beta)
cor(t)

str(individual_beta)
str(ride.lmi)
ride.lmi
conjoint1 <- subset( conjoint.df, conjoint.df$resp.id==200)

summary(lm(rating~speed+height+const+theme,data = conjoint1))


setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/CACI/WS1819/Data")
d<-read.csv("conjointdata_swp4_rating_dummy.csv")
d$EUR200<-ifelse(d$Price==200,1,0)
d$EUR250<-ifelse(d$Price==250,1,0)
d$EUR300<-ifelse(d$Price==300,1,0)
str(d)
t1<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+Price+SDCard+White+Pink+Gold, data=d)
summary(t1)
t2<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+EUR200+EUR250+SDCard+White+Pink+Gold, data=d)
summary(t2)
t3<-lm(Rating ~ SCS_52 + SCS_60 + SoftwareButton+EUR250+EUR300+SDCard+White+Pink+Gold, data=d)
summary(t3)
