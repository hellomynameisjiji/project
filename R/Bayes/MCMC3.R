setwd("D:/Dropbox/08 Jaewon/Gaon chart/2017-05-16 Analyses")
data<-read.csv("gaon_E10 170514.csv")
data<-data[data$ID!=1266,]
wag<-read.csv("cast_num3.csv")
wag$X<-NULL
wag$MBC<-as.numeric(rbind(0,matrix(wag$MBC,nrow=52))[1:52,])
wag$KBS<-as.numeric(rbind(0,matrix(wag$KBS,nrow=52))[1:52,])
wag$SBS<-as.numeric(rbind(0,matrix(wag$SBS,nrow=52))[1:52,])
wag$Mnet<-as.numeric(rbind(0,matrix(wag$Mnet,nrow=52))[1:52,])
wag$num<-as.numeric(rbind(0,matrix(wag$num,nrow=52))[1:52,])

data$period<-data$Period-153

data$num<-0
jee<-unique(wag$cast)
MMM<-matrix(wag$num,nrow=52)
for (i in 1:length(jee)){
data$num[as.character(data$artist)==as.character(jee[i])]<-MMM[,i]
print(i)
}

add<-read.csv("gaon_E12.csv")
data$new<-as.numeric(data$Period==data$r_period)
data$title<-add$title
data$ent<-add$ent
data$tv<-add$tv
data$since_debut<-2014-add$debut
data$collabo<-add$collabo
data$old<-as.numeric(data$r_period==0)


data<-data[data$old==0,]	#80756
#data<-data[data$collabo==0,]	#40048
data<-data[data$r_period>153,]
#data<-data[data$ln_lag_d>0&data$ln_lag_s>0,]
I_cut<-as.numeric(data$Period>data$r_period) #########################################################################################
#data<-data[data$Period>=data$r_period,] ## I didn't cut this, but made some indicator

data$lag_d1<-1-data$lag_d0
data$lag_s1<-1-data$lag_s0
y1<-data$ln_down
y2<-data$ln_stream
I1<-as.numeric(y1>0&I_cut==1)
I2<-as.numeric(y2>0&I_cut==1)

d1<-as.numeric(I1==1&I2==0&I_cut==1)
d2<-as.numeric(I1==0&I2==1&I_cut==1)
d3<-as.numeric(I1==0&I2==0&I_cut==1)

D1<-sum(d1)
D2<-sum(d2)
D3<-sum(d3)

gamma1<-log(data$thres_d+1)
gamma2<-log(data$thres_s+1)

data$lm_lag_d<-data$ln_lag_d
data$lm_lag_s<-data$ln_lag_s
data$lm_lag_d[data$lm_lag_d>0]<-data$lm_lag_d[data$lm_lag_d>0]-mean(data$lm_lag_d[data$lm_lag_d>0]) #########################################################################################
data$lm_lag_s[data$lm_lag_s>0]<-data$lm_lag_s[data$lm_lag_s>0]-mean(data$lm_lag_s[data$lm_lag_s>0]) #########################################################################################


data$since_r<-data$Period-data$r_period+1
data$ln_since_r<-log(data$since_r)




#######################
month<-ceiling((data$Period-153)/4.5)
month_mat<-mat.or.vec(dim(data)[1],11)
for(i in 1:11){
month_mat[,i]<-as.numeric(month==i)
}

week_mat<-mat.or.vec(dim(data)[1],51)
for(i in 1:51){
week_mat[,i]<-as.numeric(data$period==i)
}
week_mat<-week_mat[,2:51]
#######################



head(data)
pop<-log(data$weeks10+1)


x1<-cbind(data$lm_lag_d,data$lag_d1,data$lag_s1,data$ln_since_r,log(data$since_debut+1),data$title,data$ent,data$tv,week_mat) #########################################################################################
x2<-cbind(data$lm_lag_s,data$lag_d1,data$lag_s1,data$ln_since_r,log(data$since_debut+1),data$title,data$ent,data$tv,week_mat) #########################################################################################
X1<-cbind(1,data$lm_lag_s,data$lm_lag_s*data$ln_since_r,pop*data$lm_lag_s,pop,x1)
X2<-cbind(1,data$lm_lag_d,data$lm_lag_d*data$ln_since_r,pop*data$lm_lag_s,pop,x2)
Z1<-cbind(1,data$lm_lag_s)
Z2<-cbind(1,data$lm_lag_d)
X1[I_cut==0,]<-0
X2[I_cut==0,]<-0
Z1[I_cut==0,]<-0
Z2[I_cut==0,]<-0

n<-dim(X1)[1]
n2<-2*n
q1<-dim(X1)[2]
q2<-dim(X2)[2]
q<-q1+q2
en1<-1:52
en2<-53:104
kyu1<-1:q1
kyu2<-c(q1+1):q
k1<-dim(Z1)[2]
k2<-dim(Z2)[2]
k<-k1+k2
kei1<-1:k1
kei2<-c(k1+1):k
library(MCMCpack)
library(mvtnorm)

## For easier life
id_list<-unique(data$ID)
m<-length(id_list)

e<-matrix(rep(0,n2),ncol=2)
y_star<-cbind(data$ln_down,data$ln_stream)
y_star[c(data$down+data$stream)==0]<-0
y<-y_star


# Function: star_gen

star_gen<-function(d){
XB1<-apply(X1*d[,kyu1],1,sum)
XB2<-apply(X2*d[,kyu2],1,sum)

e1<-rep(0,n)
e2<-rep(0,n)

e1[I1==1]<-y1[I1==1]-XB1[I1==1]
e2[I2==1]<-y2[I2==1]-XB2[I2==1]

sd1<-sqrt(sigma[1,1])
sd2<-sqrt(sigma[2,2])
rho<-sigma[1,2]/(sd1*sd2)

u1<-runif(D1)
u2<-runif(D2)
u31<-runif(D3)
u32<-runif(D3)

e2[d1==1]<-qnorm(u1*pnorm(gamma2[d1==1]-XB2[d1==1],sd2*rho*e1[d1==1]/sd1,sqrt(1-rho^2)*sd2),sd2*rho*e1[d1==1]/sd1,sqrt(1-rho^2)*sd2)
e1[d2==1]<-qnorm(u2*pnorm(gamma1[d2==1]-XB1[d2==1],sd1*rho*e2[d2==1]/sd2,sqrt(1-rho^2)*sd1),sd1*rho*e2[d2==1]/sd2,sqrt(1-rho^2)*sd1)
e1[d3==1]<-qnorm(u31*pnorm(gamma1[d3==1]-XB1[d3==1],0,sd1),0,sd1)
e2[d3==1]<-qnorm(u32*pnorm(gamma2[d3==1]-XB2[d3==1],sd2*rho*e1[d3==1]/sd1,sqrt(1-rho^2)*sd2),sd2*rho*e1[d3==1]/sd1,sqrt(1-rho^2)*sd2)

cbind(XB1+e1,XB2+e2)
}


# Z List
Z_list<-list()
for(i in 1:m){
Z<-mat.or.vec(104,k)
Z[en1,kei1]<-Z1[data$ID==id_list[i],]
Z[en2,kei2]<-Z2[data$ID==id_list[i],]
Z_list[[i]]<-Z
print(i)
}


# Tilde function 
tilde_x<-function(t1,t2,t3,x1,x2){
rbind(cbind(t1*x1,0*x2),cbind(t2*x1,t3*x2))
}
tilde_y<-function(t1,t2,t3,x1,x2){
c(t1*x1,t2*x1+t3*x2)
}
tilde_z<-function(x){
rbind(cbind(t1*x[en1,kei1],0*x[en2,kei2]),cbind(t2*x[en1,kei1],t3*x[en2,kei2]))
}


# Priors

sigma<-diag(rep(4,2))
nu<-dim(sigma)[1]+3
V<-nu*diag(rep(0.1,dim(sigma)[1]))

b_pop_v<-diag(rep(1000,q))
b_pop_prec<-solve(b_pop_v)
b_pop<-mvrnorm(1,rep(0,q),b_pop_prec)
b_pop[1]<-b_pop[1]+8
b_pop[q1+1]<-b_pop[q1+1]+12

omega<-diag(rep(1,k))
nu0<-dim(omega)[1]+3
V0<-nu0*diag(rep(0.1,dim(omega)[1]))

ai<-mvrnorm(m,rep(0,k),omega)
AI<-matrix(rep(ai,each=52),nrow=n)
bi_mat<-matrix(rep(b_pop,each=n),nrow=n)
bi_mat[,1:k1]<-bi_mat[,1:k1]+AI[,1:k1]
bi_mat[,c(q1+1):c(q1+k2)]<-bi_mat[,c(q1+1):c(q1+k2)]+AI[,c(k1+1):k]
bi_mat[I_cut==0,]<-0



# Iteration: Real

figure<-c(1:4,q1+c(1:4),q+8)
length(figure)
IT<-15000
parameters<-mat.or.vec(IT/10,q+8)
bi_array<-array(0,c(IT/10,m,4))

for (j in 1:IT){

## Sample y_star
y_star<-star_gen(bi_mat)

## Cholesky decomp
tilde<-t(solve(chol(sigma)))
t1<-tilde[1,1]
t2<-tilde[2,1]
t3<-tilde[2,2]
y_tilde<-tilde_y(t1,t2,t3,y_star[,1],y_star[,2])
X_tilde<-tilde_x(t1,t2,t3,X1,X2)
Z_tilde<-lapply(Z_list,tilde_z)
X<-tilde_x(1,0,1,X1,X2)


## Sample b_pop

AI1<-rowSums(Z1*AI[,kei1])
AI2<-rowSums(Z2*AI[,kei2])
yb_tilde<-y_tilde-tilde_y(t1,t2,t3,AI1,AI2)
XX_tilde<-crossprod(X_tilde)
XY_tilde<-as.numeric(t(X_tilde)%*%yb_tilde)
b_pop_pv<-solve(XX_tilde+b_pop_prec)
b_pop_pm<-as.numeric(b_pop_pv%*%XY_tilde)
b_pop<-mvrnorm(1,b_pop_pm,b_pop_pv)
B_POP<-matrix(rep(b_pop,each=n),nrow=n)


## Sample ai
ZZ<-lapply(Z_tilde,crossprod)
ai_pv<-lapply(ZZ,function(x){solve(x+solve(omega))})
ya_tilde<-y_tilde-as.numeric(X_tilde%*%b_pop)
ya_tilde_mat<-matrix(ya_tilde,ncol=2)
ya_tilde1<-ya_tilde_mat[,1]
ya_tilde2<-ya_tilde_mat[,2]
ya_tilde<-lapply(apply(rbind(matrix(ya_tilde1,nrow=52),matrix(ya_tilde2,nrow=52)),2,list),unlist)
ai_pm0<-lapply(apply(mapply(crossprod,Z_tilde,ya_tilde),2,list),unlist)
ai_pm<-t(mapply(crossprod,ai_pv,ai_pm0))
ai<-ai_pm+t(mapply(function(x){rmvnorm(1,c(0,0,0,0),x)},x=ai_pv))


## Sample omega
omega<-riwish(nu0+m-16,t(ai)%*%ai+V0)


## Sample sigma
AI<-matrix(rep(ai,each=52),n)
bi_mat<-B_POP
bi_mat[,1:2]<-bi_mat[,1:2]+AI[,1:2]
bi_mat[,c(q1+1):c(q1+2)]<-bi_mat[,c(q1+1):c(q1+2)]+AI[,c(k1+1):k]
bi_mat[I_cut==0,]<-0
E<-y_star[I_cut==1,]-cbind(rowSums(X1[I_cut==1,]*bi_mat[I_cut==1,1:q1]),rowSums(X2[I_cut==1,]*bi_mat[I_cut==1,c(q1+1):q]))
sigma<-riwish(nu+dim(E)[1],t(E)%*%E+V)

j10<-j/10
j100<-j/100
j1000<-j/1000

if(round(j10)==j10){
parameters[j10,]<-c(b_pop,sqrt(diag(omega)),sqrt(diag(sigma)),sigma[1,2]/sqrt(prod(diag(sigma))),sum(dmvnorm(E,c(0,0),sigma,log=TRUE)))
bi_array[j10,,1]<-ai[,1]+b_pop[1]
bi_array[j10,,2]<-ai[,2]+b_pop[2]
bi_array[j10,,3]<-ai[,3]+b_pop[q1+1]
bi_array[j10,,4]<-ai[,4]+b_pop[q1+2]
}

if(round(j100)==j100){
par(mar=c(2.5,2.5,1,1))
par(mfrow=c(3,4))
for(pr in 1:length(figure)){
plot(parameters[1:j10,figure[pr]],type="l")
abline(h=0,col=2)
}
}

if(round(j/1000)==j/1000){
write.csv(parameters[1:j10,],"D:/Dropbox/08 Jaewon/Gaon chart/2017-05-16 Analyses/MCMC3.csv")
}

print(j)
}

save.image("D:/Dropbox/08 Jaewon/Gaon chart/2017-05-16 Analyses/MCMC3.RData")



##

sam_sel<-501:1500

conf90<-function(x){
quantile(x,c(0.05,0.95))
}
star90<-t(apply(parameters[sam_sel,],2,conf90))

conf95<-function(x){
quantile(x,c(0.025,0.975))
}
star95<-t(apply(parameters[sam_sel,],2,conf95))

conf99<-function(x){
quantile(x,c(0.005,0.995))
}
star99<-t(apply(parameters[sam_sel,],2,conf99))

star<-as.numeric(apply(star90,1,prod)>0)+as.numeric(apply(star95,1,prod)>0)+as.numeric(apply(star99,1,prod)>0)
output<-cbind(apply(parameters[sam_sel,],2,median),star,star90,star95,star99)


## DIC
ll<-rep(0,1000)
theta1<-apply(bi_array[sam_sel,,1],2,mean)
theta2<-apply(bi_array[sam_sel,,2],2,mean)
theta3<-apply(bi_array[sam_sel,,3],2,mean)
theta4<-apply(bi_array[sam_sel,,4],2,mean)
theta_mat<-matrix(rep(output[1:q,1],each=n),nrow=n)
theta_mat[,1]<-rep(theta1,each=52)
theta_mat[,2]<-rep(theta2,each=52)
theta_mat[,c(q1+1)]<-rep(theta3,each=52)
theta_mat[,c(q1+2)]<-rep(theta4,each=52)

XB1<-apply(X1*theta_mat[,kyu1],1,sum)
XB2<-apply(X2*theta_mat[,kyu2],1,sum)
e1<-rep(0,n)
e2<-rep(0,n)
e1[I1==1&I_cut==1]<-y1[I1==1&I_cut==1]-XB1[I1==1&I_cut==1]
e2[I2==1&I_cut==1]<-y2[I2==1&I_cut==1]-XB2[I2==1&I_cut==1]
sd1<-output[q+5]
sd2<-output[q+6]
rho<-output[q+7]
sigma<-diag(c(sd1,sd2)^2)
sigma[1,2]<-sd1*sd2*rho
sigma[2,1]<-sd1*sd2*rho


# Function: star_gen2
star_gen2<-function(w){
set.seed(w*111)
u1<-runif(D1)
set.seed(w*202)
u2<-runif(D2)
set.seed(w*330)
u31<-runif(D3)
set.seed(w*400)
u32<-runif(D3)
e2[d1==1]<-qnorm(u1*pnorm(gamma2[d1==1]-XB2[d1==1],sd2*rho*e1[d1==1]/sd1,sqrt(1-rho^2)*sd2),sd2*rho*e1[d1==1]/sd1,sqrt(1-rho^2)*sd2)
e1[d2==1]<-qnorm(u2*pnorm(gamma1[d2==1]-XB1[d2==1],sd1*rho*e2[d2==1]/sd2,sqrt(1-rho^2)*sd1),sd1*rho*e2[d2==1]/sd2,sqrt(1-rho^2)*sd1)
e1[d3==1]<-qnorm(u31*pnorm(gamma1[d3==1]-XB1[d3==1],0,sd1),0,sd1)
e2[d3==1]<-qnorm(u32*pnorm(gamma2[d3==1]-XB2[d3==1],sd2*rho*e1[d3==1]/sd1,sqrt(1-rho^2)*sd2),sd2*rho*e1[d3==1]/sd1,sqrt(1-rho^2)*sd2)
cbind(e1,e2)[I_cut==1,]
}

ll<-rep(0,1000)
for(w in 1:1000){
E<-star_gen2(w)
ll[w]<-sum(dmvnorm(E,c(0,0),sigma,log=TRUE))
print(w)
}

DIC<-2*mean(ll)-4*mean(parameters[sam_sel,dim(parameters)[2]]) 
DIC # -130045.6

write.csv(output,"D:/Dropbox/08 Jaewon/Gaon chart/Final results for write-up/Spillover/MCMC output15.csv")

















############################################## Iteration: Initial ##############################################

figure<-c(1:4,q1+c(1:4),q+c(1:4))
length(figure)
IT<-11000
parameters<-mat.or.vec(IT/10,q+4)

for (j in 1:IT){

## Sample y_star
y_star<-star_gen(bi_mat)

## Cholesky decomp
tilde<-t(solve(chol(sigma)))
t1<-tilde[1,1]
t2<-tilde[2,1]
t3<-tilde[2,2]

## Sample b_pop
X_tilde<-cbind(rbind(t1*X1,t2*X1),rbind(0*X2,t3*X2))
y1b<-y_star[,1]
y2b<-y_star[,2]
yb_tilde<-c(t1*y1b,t2*y1b+t3*y2b)
XX_tilde<-crossprod(X_tilde)
XY_tilde<-as.numeric(t(X_tilde)%*%yb_tilde)
b_pop_pv<-solve(XX_tilde+b_pop_prec)
b_pop_pm<-as.numeric(b_pop_pv%*%XY_tilde)
b_pop<-mvrnorm(1,b_pop_pm,b_pop_pv)
B_POP<-matrix(rep(b_pop,each=n),nrow=n)

## Sample sigma
bi_mat<-B_POP
bi_mat[I_cut==0,]<-0
E<-y_star[I_cut==1,]-cbind(rowSums(X1[I_cut==1,]*bi_mat[I_cut==1,1:q1]),rowSums(X2[I_cut==1,]*bi_mat[I_cut==1,c(q1+1):q]))
sigma<-riwish(nu+dim(E)[1],t(E)%*%E+V)

j10<-j/10
j100<-j/100
j1000<-j/1000

if(round(j10)==j10){
parameters[j10,]<-c(b_pop,sqrt(diag(sigma)),sigma[1,2]/sqrt(prod(diag(sigma))),sum(dmvnorm(E,c(0,0),sigma,log=TRUE)))
}

if(round(j100)==j100){
par(mar=c(2.5,2.5,1,1))
par(mfrow=c(4,3))
for(pr in 1:length(figure)){
plot(parameters[1:j10,figure[pr]],type="l")
abline(h=0,col=2)
}
}

print(j)
}

write.csv(init,"initial values.csv")
b_pop<-init

