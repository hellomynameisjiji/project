ls()

###################
## Time Commerce ##
###################

dim(thirty)
head(thirty)
summary(thirty)

dev.new(width=8,height=8)
par(mfrow=c(3,3))
for(i in 2:10){
hist(thirty[,i],main=as.character(names(thirty)[i]))
} ## Need to take log on Sales

windows()
plot(thirty$Remain,thirty$Sales)
plot(aggregate(thirty$Sales,by=list(thirty$Remain),mean),type="l")

range(thirty$Sales)
plot(thirty$Remain,log(thirty$Sales+1))
plot(aggregate(log(thirty$Sales+1),by=list(thirty$Remain),mean),type="l")

mc<-function(x){
x-mean(x)
}


## 1a. Intercept - Observed heterogeneity

model0<-lm(Sales~mc(Remain)+promotion,thirty) ## No significance if we don't take log
summary(model0)

model0<-lm(log(Sales+1)~mc(Remain)+promotion,thirty)
summary(model0)

hist(apply(matrix(resid(model0),ncol=200),2,mean)) ## Error correlation

model1<-lm(log(Sales+1)~(Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+mc(Remain)+promotion,thirty)
summary(model1)

hist(apply(matrix(resid(model1),ncol=200),2,mean)) ## A lot alleviated

anova(model0,model1)



## 1b. Intercept - Unobserved heterogeneity

## Random effect
install.packages("lme4")
library(lme4)
model2<-lmer(log(Sales+1)~((1|ID)+Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+mc(Remain)+promotion,thirty)
summary(model2)

## Fixed effect

model3<-lm(log(Sales+1)~as.factor(ID)+(Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+(mc(Remain)+promotion),thirty)
summary(model3)

model3<-lm(log(Sales+1)~as.factor(ID)+(mc(Remain)+promotion),thirty)
summary(model3)


par(mfrow=c(2,2))
hist(apply(matrix(resid(model0),ncol=200),2,mean))
hist(apply(matrix(resid(model1),ncol=200),2,mean))
hist(apply(matrix(resid(model2),ncol=200),2,mean))
hist(apply(matrix(resid(model3),ncol=200),2,mean))



## 2a. Slopes - Observed heterogeneity

# Observed intercept + Observed slope
model4<-lm(log(Sales+1)~(Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+mc(Remain)*Vice+promotion,thirty)
summary(model4)

model4<-lm(log(Sales+1)~(Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+Vice*(mc(Remain)+promotion),thirty)
summary(model4)

thirty$virtue<- 1-thirty$Vice## Setting Vice as the baseline

model4<-lm(log(Sales+1)~(Vice+virtue+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+virtue*(mc(Remain)+promotion),thirty)
summary(model4) ## Drop vice

model4<-lm(log(Sales+1)~(virtue+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+virtue*(mc(Remain)+promotion),thirty)
summary(model4) ## Drop vice

hist(apply(matrix(resid(model4),ncol=200),2,mean))

# Random intercept + Observed slope
model5<-lmer(log(Sales+1)~((1|ID)+Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+Vice*(mc(Remain)+promotion),thirty)
summary(model5)

# Fixed intercept + Observed slope
model6<-lm(log(Sales+1)~as.factor(ID)+Vice*(mc(Remain)+promotion)-Vice,thirty)
summary(model6)



## 2b. Slopes - Unobserved heterogeneity

# Random intercept + Random slope
model7<-lmer(log(Sales+1)~(Vice+mc(total_exp)+Snack+Airtight+Domestic+mc(price))+Vice*(mc(Remain)+promotion)+(mc(Remain)+promotion|ID),thirty)
summary(model7)

# Fixed intercept + Random slope
model8<-lmer(log(Sales+1)~as.factor(ID)+(mc(Remain)+promotion-1|ID)+Vice*(mc(Remain)+promotion)-Vice,thirty)
summary(model8)

# Fixed intercept + fixed slope
model9<-lm(log(Sales+1)~as.factor(ID)*(mc(Remain)+promotion),thirty)
summary(model9) ## WHy NA's for promotion, extremecase...


output<-NULL
for(i in 1:200){
output<-rbind(output,coef(lm(log(Sales+1)~mc(Remain)+promotion,thirty[thirty$ID==i,])))
print(i)
}

output # Why do we observe so many zeros?
hist(output[output[,1]!=0,1])
hist(output[output[,1]!=0,2]) ## We can check out the distribution of coefficients.
## NA for no variation
## 0 for no sales at all.






###################
## Crowd funding ##
###################

head(cf)
dim(cf)
summary(cf)

cor(cf[,-c(1,2,6)])
pairs(cf[,-c(1,2,6)])

dev.new(width=8,height=8)
par(mfrow=c(3,2))
hist(cf$t)
hist(cf$daily_per)
hist(cf$cum_per)
hist(cf$weekend)
hist(cf$publisher)

windows()
plot(aggregate(cf$daily_per,by=list(cf$t),mean),type="l")
cf$lag_cum_per<-cf$cum_per-cf$daily_per ## This is what we want to test.

cf$stage<-as.numeric(cf$lag_cum_per>0.2)+as.numeric(cf$lag_cum_per>0.4)+as.numeric(cf$lag_cum_per>0.6)+as.numeric(cf$lag_cum_per>0.8)
plot(aggregate(cf$daily_per,by=list(cf$stage),mean),type="l")


mc<-function(x){
x-mean(x)
}

model0<-lm(daily_per~mc(lag_cum_per),cf) ## No mean-center if 0 is in the range of IVs
summary(model0)

cf$lag_cum_per
cf$lag_cum_sq<-cf$lag_cum_per^2

model0<-lm(daily_per~lag_cum_per+lag_cum_sq,cf)
summary(model0)
windows()
x<-seq(0,1,length=1000)
plot(x,coef(model0)[1]+coef(model0)[2]*x+coef(model0)[3]*x^2,type="l")
abline(v=-coef(model0)[2]/(2*coef(model0)[3]),col=2) ## The lowest point

head(cf)
cf$t<-cf$t-1
model1<-lm(daily_per~lag_cum_per+lag_cum_sq+weekend+type+t+mc(log(publisher)),cf)
summary(model1)

cf$t_sq<-cf$t^2
model2<-lm(daily_per~lag_cum_per+lag_cum_sq+weekend+type+t+t_sq+mc(log(publisher)),cf)
summary(model2)

model3<-lm(daily_per~as.factor(project_id)+lag_cum_per+lag_cum_sq+weekend+t+t_sq,cf)
summary(model3)

library(lme4)
model4<-lmer(daily_per~(1|project_id)+lag_cum_per+lag_cum_sq+weekend+type+t+t_sq+mc(log(publisher)),cf)
summary(model4)

model5<-lm(daily_per~as.factor(project_id)+(lag_cum_per+lag_cum_sq)*type+weekend+t+t_sq-type,cf)
summary(model5)

anova(model3,model5)
x<-seq(0,1,length=1000)

base<-c(coef(model5)[1],coef(model5)[1]+coef(model5)[2:63])
type<-cf$type[cf$t==0]
base_u<-mean(base[type=="util"])
base_h<-mean(base[type=="hed"])

hed<-base_h+coef(model5)[64]*x+coef(model5)[65]*x^2
util<-base_u+(coef(model5)[64]+coef(model5)[69])*x+(coef(model5)[65]+coef(model5)[70])*x^2
matplot(x,cbind(util,hed),type="l",col=c(1,2))

model6<-lmer(daily_per~(1|project_id)+(lag_cum_per+lag_cum_sq)*type+weekend+type+t+t_sq+mc(log(publisher)),cf)
summary(model6)

anova(model4,model6)

x<-seq(0,1,length=1000)
beta<-apply(coef(model6)$project_id,2,mean) # Obtaining mean effect
hed<-beta[1]+beta[2]*x+beta[3]*x^2
util<-beta[1]+beta[4]+(beta[2]+beta[9])*x+(beta[3]+beta[10])*x^2
matplot(x,cbind(util,hed),type="l",col=c(1,2))

## Given that our goal is not the hypotheses testing, I wound not like to run individual regression for each individual.



##########
## beer ##
##########

head(beer) ## In conjoint data, you use dummy variables for your analyses ##
summary(beer)

model0<-lm(rating~type+ABV+size,beer)
summary(model0)


## Part worth plot
plot(c(0,coef(model0)[2:3]),type="b")
plot(c(0,coef(model0)[4:5]),type="b")
plot(c(coef(model0)[6:7],0),type="b")


## Relative importance
type<-max(c(0,coef(model0)[2:3]))-min(c(0,coef(model0)[2:3]))
abv<-max(c(0,coef(model0)[4:5]))-min(c(0,coef(model0)[4:5]))
size<-max(c(0,coef(model0)[6:7]))-min(c(0,coef(model0)[6:7]))
sum<-type+abv+size

type/sum
abv/sum
size/sum


## Individual regression

results<-NULL
for(i in 1:20){
results<-rbind(results,coef(lm(rating~type+ABV+size,beer[beer$id==i,])))
}
results


## Individual relative importance

type<-apply(cbind(0,results[,2:3]),1,max)-apply(cbind(0,results[,2:3]),1,min)
abv<-apply(cbind(0,results[,4:5]),1,max)-apply(cbind(0,results[,4:5]),1,min)
size<-apply(cbind(0,results[,6:7]),1,max)-apply(cbind(0,results[,6:7]),1,min)
sum<-type+abv+size

hist(type/sum)
hist(abv/sum)
hist(size/sum)

cbind(type/sum,abv/sum,size/sum) ## WHy can't we do this with regular regressions?

plot(type/sum,size/sum)

plot(type/sum,abv/sum,type="n")
text(type/sum,abv/sum,labels=1:20,cex=1)
plot(type/sum,size/sum,type="n")
text(type/sum,size/sum,labels=1:20,cex=1)
plot(abv/sum,size/sum,type="n")
text(abv/sum,size/sum,labels=1:20,cex=1)

apply(cbind(type/sum,abv/sum,size/sum),1,which.max)
c(1:20)[apply(cbind(type/sum,abv/sum,size/sum),1,which.max)==1] # Type seeker
c(1:20)[apply(cbind(type/sum,abv/sum,size/sum),1,which.max)==2] # ABV seeker
c(1:20)[apply(cbind(type/sum,abv/sum,size/sum),1,which.max)==3] # Size seeker. But, it does not necessarily mean that "what type" or "what size" one prefers.


## Predicting market share
cbind(results[,2]+0+results[,6],0+results[,4]+results[,7],results[,3]+results[,5]+0)
apply(cbind(results[,2]+0+results[,6],0+results[,4]+results[,7],results[,3]+results[,5]+0),1,which.max)
table(apply(cbind(results[,2]+0+results[,6],0+results[,4]+results[,7],results[,3]+results[,5]+0),1,which.max))/20
