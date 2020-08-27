ls()

##################
## Push message ##
##################

dim(push)
head(push)
cor(push[,-c(2)])
pairs(push[,-c(2)])

dev.new(height=8,width=12)
par(mfrow=c(2,3))

fig<-c(1,3,4,5,6,7)
for(i in 1:6){
hist(push[,fig[i]],main=as.character(names(push)[fig[i]]))
}

mc<-function(x){
x-mean(x)
}

## Linear regression
model0<-lm(convergence~mc(age)+gender+mc(past)+as.factor(time)+as.factor(subway)+crowdedness,push)
summary(model0)
hist(fitted(model0))

## Logistic transformation 1
p<-seq(0,1,length=1000)
plot(p,log(p/(1-p)))

## Logistic transformation 2
x<- seq(-10,10,length=1000)
plot(x,exp(x)/(exp(x)+1))

## Let's go back to the data
model1<-glm(convergence~mc(age)+gender+mc(past)+as.factor(time)+as.factor(subway)+mc(crowdedness),family="binomial",push)
summary(model1)
hist(fitted(model1))
## Interpretation of the coefficient
## Hypothesis testing
## Model fit
predicted<-as.numeric(fitted(model1)>0.5)
table(predicted,push$convergence)
sum(diag(table(predicted,push$convergence)))/1000
model1$deviance

## Would the effect of crowdedness vary across gender?
model2<-glm(convergence~mc(age)+gender+mc(past)+as.factor(time)+as.factor(subway)+mc(past)+gender*mc(crowdedness),family="binomial",push)
summary(model2)
model2$deviance
model1$deviance-model2$deviance
1-pchisq((model1$deviance-model2$deviance),1)
anova(model1,model2)


## Managerial calculation
u<-coef(model1)[1]+coef(model1)[3]+coef(model1)[9]+coef(model1)[15]+coef(model1)[4]*(360-mean(push$past))+coef(model1)[2]*(30-mean(push$age))+coef(model1)[18]*(3-mean(push$crowdedness))
pr<-exp(u)/(1+exp(u))
10*pr-1

u<-coef(model1)[1]+coef(model1)[8]+coef(model1)[10]+coef(model1)[4]*(420-mean(push$past))+coef(model1)[2]*(27-mean(push$age))+coef(model1)[18]*(5-mean(push$crowdedness))
pr<-exp(u)/(1+exp(u))
10*pr-1

## Did not consider the prob that one play with no push message


#########
## Gym ##
#########

head(gym)
dim(gym)
T<-table(gym$motivation,gym$y)
rbind(T[1,]/sum(T[1,]),T[2,]/sum(T[2,]))
chisq.test(T,1)
gym$weekend<-as.numeric(weekdays(as.Date(gym$t))=="토요일"|weekdays(as.Date(gym$t))=="일요일")

head(gym)
gym$since<-log(as.numeric(gym$t))
model0<-glm(y~mc(age)+gender+mc(temp)+mc(bmi)+weather+motivation+weekend+since,family="binomial",gym)
summary(model0)

## Random effect on binary choice
## We are not allowed to used FE in logit model!!
library(lme4)
model1<-glmer(y~(1|id)+mc(age)+gender+mc(temp)+mc(bmi)+weather+motivation+weekend+since,family="binomial",gym)
summary(model1)

pred<-as.numeric(fitted(model1)>0.5)
table(pred,gym$y)
sum(gym$y)/dim(gym)[1]
sum(diag(table(pred,gym$y)))/dim(gym)[1] ## Hit rate

## Interactive effect
gym$hard<-as.numeric(gym$weather=="rain"|gym$temp>35)

model2<-glm(y~mc(age)+gender+mc(temp)+mc(bmi)+weather+motivation*hard+weekend+since,family="binomial",gym)
summary(model2)

model3<-glmer(y~(1|id)+mc(age)+gender+mc(temp)+mc(bmi)+weather+motivation*hard+weekend+since,family="binomial",gym)
summary(model3)


##########
## Jeju ##
##########

head(jeju)
hist(jeju$age)
hist(jeju$income)
aggregate(jeju$y,by=list(jeju$price),mean)
aggregate(jeju$y,by=list(jeju$type),mean)
aggregate(jeju$y,by=list(jeju$location),mean)


## 1. Model
model0<-glm(y~location+type+as.factor(price),family="binomial",jeju)
summary(model0)
model1<-glm(y~mc(age)+mc(income)+gender+location+type+as.factor(price),family="binomial",jeju)
summary(model1)
model2<-glmer(y~(1|id)+mc(age)+mc(income)+gender+location+type+as.factor(price),family="binomial",jeju)
summary(model2)


## 2. Relative importance

## Aggregate model
summary(model1)
loc_imp<-abs(coef(model1)[5])
type_imp<-coef(model1)[7]-coef(model1)[6]
price_imp<-coef(model1)[9]-coef(model1)[8]
c(loc_imp,type_imp,price_imp)/(loc_imp+type_imp+price_imp)

summary(model2)
loc_imp<-abs(coef(model2)[[1]][1,5])
type_imp<-coef(model2)[[1]][1,7]-coef(model2)[[1]][1,6]
price_imp<-coef(model2)[[1]][1,9]-coef(model2)[[1]][1,8]
c(loc_imp,type_imp,price_imp)/(loc_imp+type_imp+price_imp)

## Individual model
indiv_results<-NULL
for(i in 1:100){
indiv_results<-rbind(indiv_results,coef(glm(y~location+type+as.factor(price),family="binomial",jeju[jeju$id==i,])))
print(i)
}
indiv_results

location_imp<-abs(indiv_results[,2])
type_imp<-apply(cbind(0,indiv_results[,3:4]),1,max)-apply(cbind(0,indiv_results[,3:4]),1,min)
price_imp<-apply(cbind(0,indiv_results[,5:6]),1,max)-apply(cbind(0,indiv_results[,5:6]),1,min)

rel_imp<-cbind(location_imp,type_imp,price_imp)
rel_imp/rowSums(rel_imp)

hist(rel_imp[,1]/rowSums(rel_imp))
hist(rel_imp[,2]/rowSums(rel_imp))
hist(rel_imp[,3]/rowSums(rel_imp))

## In addition. Elevator version of segmentation, and identifying who they are.

who<-apply(rel_imp,1,which.max)
who[who==1]<-"location"
who[who==2]<-"type"
who[who==3]<-"price"
jeju$q<-rep(1:18,100)
aggregate(jeju$age[jeju$q==1],by=list(who),mean)
aggregate(as.numeric(jeju$gender[jeju$q==1]=="M"),by=list(who),mean)
aggregate(jeju$income[jeju$q==1],by=list(who),mean)


### 3.Market share

## Aggregate
jae<-coef(model1)[1]+coef(model1)[5]+coef(model1)[8]
comp1<-coef(model1)[1]+coef(model1)[5]
comp2<-coef(model1)[1]+coef(model1)[5]+coef(model1)[7]+coef(model1)[8]
c(jae,comp1,comp2) ## Max rule: Comp1 100%
exp(c(jae,comp1,comp2))/sum(exp(c(jae,comp1,comp2))) ##Logit rule


## Individual
jae<-indiv_results[,1]+indiv_results[,2]+indiv_results[,5]
comp1<-indiv_results[,1]+indiv_results[,2]
comp2<-indiv_results[,1]+indiv_results[,2]+indiv_results[,4]+indiv_results[,6]

cbind(jae,comp1,comp2)
apply(cbind(jae,comp1,comp2),1,which.max)
as.numeric(apply(cbind(jae,comp1,comp2),1,which.max)==1)
mean(as.numeric(apply(cbind(jae,comp1,comp2),1,which.max)==1))  ### Max rule
mean(exp(jae)/(exp(jae)+exp(comp1)+exp(comp2))) ## Logit rule


### 4. Demand curve
### I will assume that competitors do not react to my pricing policy.
### Price sensitivity between 150~200
### Price sensitivity between 200~250 => Different

## Aggregate
comp1<-coef(model1)[1]+coef(model1)[5]
comp2<-coef(model1)[1]+coef(model1)[5]+coef(model1)[7]+coef(model1)[8]

x1<-0:50
x2<-1:50

jae1<-coef(model1)[1]+coef(model1)[5]+x1*coef(model1)[8]/50
jae2<-coef(model1)[1]+coef(model1)[5]+coef(model1)[8]+x2*(coef(model1)[9]-coef(model1)[8])/50
jae<-c(jae1,jae2)

E1<-exp(comp1)
E2<-exp(comp2)
exp(jae)/(exp(jae)+E1+E2)
plot(c(x1+150,x2+200),exp(jae)/(exp(jae)+E1+E2),type="l")  ## Go with 250


## Individual

x1<-0:50
x2<-1:50
D<-rep(0,101)

for(i in 1:100){
comp1<-indiv_results[i,1]+indiv_results[i,2]
comp2<-indiv_results[i,1]+indiv_results[i,2]+indiv_results[i,4]+indiv_results[i,6]

jae1<-indiv_results[i,1]+indiv_results[i,2]+x1*indiv_results[i,5]/50
jae2<-indiv_results[i,1]+indiv_results[i,2]+indiv_results[i,5]+x2*(indiv_results[i,6]-indiv_results[i,5])/50
jae<-c(jae1,jae2)

E1<-exp(comp1)
E2<-exp(comp2)
d<-exp(jae)/(exp(jae)+E1+E2)
D<-D+d
}

plot(c(x1+150,x2+200),D,type="l")  ## Go with 200: Magic of aggregation -> Non linearity + Aggregation makes things bumpy





####################
### idol example ###
####################

head(idol)
dim(idol)
idol$debut
idol$disband
idol$duration<-2018-idol$debut
idol$duration[is.na(idol$disband)==FALSE]<-idol$disband[is.na(idol$disband)==FALSE]+1-idol$debut[is.na(idol$disband)==FALSE]

mc<-function(x){
x-mean(x)
}

model0<-lm(duration~mc(debut)+mc(mem)+gender,idol)
summary(model0)

max(idol$duration)
dim(idol)
y<-rep(0,271*24) ## Making a matrix
y<-matrix(y,nrow=24)
for(i in 1:271){
y[idol$duration[i],i]<-1
print(i)
} ## y is for the last observation

y_cum<-apply(y,2,cumsum) ##
y_cum<-apply(y_cum,2,cumsum) ## To cut obs after adoption / after 2017
y[,is.na(idol$disband)==TRUE]<-0 ## Last observation, but disband has not happended
cbind(as.numeric(y),as.numeric(y_cum))

name<-rep(idol$name,each=24)
mem<-rep(idol$mem,each=24)
debut<-rep(idol$debut,each=24)
gender<-rep(idol$gender,each=24)
since<-rep(1:24,271)
y<-as.numeric(y)

idol2<-data.frame(name,y,mem,debut,gender,since)
idol2<-idol2[as.numeric(y_cum)<2,]

model1<-glm(y~mc(mem)+mc(debut)+gender+since,family="binomial",idol2)
summary(model1)

idol2$since2<-idol2$since^2
model2<-glm(y~mc(mem)+mc(debut)+gender+since+since2,family="binomial",idol2)
summary(model2)

model3<-glm(y~mc(mem)+mc(debut)+gender+as.factor(since),family="binomial",idol2)
summary(model3)
plot(c(0,coef(model3)[5:28]),type="b")

