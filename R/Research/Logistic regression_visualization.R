######################################################################################
####### This is a sample of my works to show you how I've worked on analysis ########
######################################################################################


#write.csv(multi5, "Final multi.csv")
#write.csv(single3, "Final single.csv")
m_multi2<-glm(sum~(mc_rank_rate+mc_sq_rank_rate+new_freq+log(freq_freq+1)+log(recency+1)+log(wratio*100+1)+log(first_time2)+current+google+log(new+1)+game_time3)*update_yn+as.factor(title), family="binomial"(link="probit"),multi5)
summary(m_multi2)
m_multi2_2<-glm(sum~(mc_rank_rate2+mc_sq_rank_rate2+new_freq+log(freq_freq+1)+log(recency+1)+log(wratio*100+1)+log(first_time2)+current+google+log(new+1)+game_time3)*update_yn+as.factor(title), family="binomial"(link="probit"),multi5)
summary(m_multi2_2)
m_multi2_r<-glmer(sum~(mc_rank_rate+mc_sq_rank_rate+new_freq+log(freq_freq+1)+log(recency+1)+log(wratio*100+1)+log(first_time2)+current+google+log(new+1)+game_time3)*update_yn+as.factor(title)+(1|combi2), family="binomial"(link="probit"),multi5)
summary(m_multi2_r)
m_single<-glm(sum~(mc_rank_rate+mc_sq_rank_rate+new_freq+log(freq_freq+1)+log(recency+1)+log(wratio*100+1)+log(first_time2)+current+google+log(new+1)+game_time3)*update_yn+as.factor(title2),family="binomial"(link="probit"),single3)
summary(m_single)
library("lme4")
m_multi2_2_r<-glmer(sum~(mc_rank_rate2+mc_sq_rank_rate2+new_freq+log(freq_freq+1)+log(recency+1)+log(wratio*100+1)+log(first_time2)+current+google+log(new+1)+game_time3)*update_yn+as.factor(title)+(1|combi2), family="binomial"(link="probit"),multi5)
summary(m_multi2_2_r)

colnames(multi5)
## Main Model Graph ##
summary(m_multi2_2)

## Graph ##

length(coef(m_multi2))
cbind(1:41,coef(m_multi2)) ## What are "rtt2" and "game_time"? Why no interaction with udpate?


## Assessing significamce of "After" estimates
n1<-1:12
n2<-c(13,31:41)

est<-coef(m_multi2)[n1]+coef(m_multi2)[n2]
se0<-diag(vcov(m_multi2))[n1]+diag(vcov(m_multi2))[n2]
for(i in 1:12){
se0[i]<-se0[i]+2*vcov(m_multi2)[n1[i],n2[i]]
print(i)
}
se<-sqrt(se0)
t<-est/se
p<-2*(1-pt(abs(t),560519))

cbind(est,se,t,p)


## Social Status and Gaming Behavior 1: Before Updates
x<-seq(-0.5,0.5,length=1000)
EV0<-as.numeric(cbind(x^2,x)%*%coef(m_multi2)[3:2])
w0<-which.max(EV0)
x0<-x[w0]
a0<-EV0[w0]
EV<-as.numeric(cbind(x^2,x)%*%coef(m_multi2)[3:2])-a0
matplot(x,cbind(EV0,EV),type="l",lty=c(1,2,2),col=1)
abline(h=0,col=2)

SE<-sqrt(vcov(m_multi2)[3,3]*x^4+vcov(m_multi2)[3,2]*2*x^3+vcov(m_multi2)[2,2]*x^2+
vcov(m_multi2)[3,3]*x0^4+vcov(m_multi2)[3,2]*2*x0^3+vcov(m_multi2)[2,2]*x0^2-
2*(vcov(m_multi2)[3,3]*x^2*x0^2+vcov(m_multi2)[2,2]*x*x0+vcov(m_multi2)[3,2]*(x^2*x0+x*x0^2)))
 dev.new(width=5, height=2.5, unit="in")
 png("multi-bf_new.png")
results<-cbind(EV,EV-1.96*SE,EV+1.96*SE)
matplot(x,results,type="l",lty=c(1,2,2),col=1, xlab=expression(italic("Player Rank")), ylab=expression(italic("Expected Utility of Gameplay")),lwd=1, axes=F)
axis(1, at=c(-0.4, -0.2, 0, 0.2, 0.4), lab=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2)
abline(h=0,col=2)
legend("bottomright", legend=c("Estimated Utility", "95% CI"), lty=1:2)
dev.off()


## Social Status and Gaming Behavior 2: After Updates
fuck<-coef(m_multi2)[32:31]+coef(m_multi2)[3:2]
EV0<-as.numeric(cbind(x^2,x)%*%fuck)
w0<-which.min(EV0)
x0<-x[w0]
a0<-EV0[w0]
EV<-as.numeric(cbind(x^2,x)%*%fuck)-a0
matplot(x,cbind(EV0,EV),type="l",lty=c(1,2,2),col=1)
abline(h=0,col=2)

fuck_sq<-vcov(m_multi2)[3,3]+vcov(m_multi2)[32,32]+2*vcov(m_multi2)[3,32]
fuck_main<-vcov(m_multi2)[2,2]+vcov(m_multi2)[31,31]+2*vcov(m_multi2)[2,31]
fuck_cov<-vcov(m_multi2)[3,2]+vcov(m_multi2)[3,31]+vcov(m_multi2)[32,2]+vcov(m_multi2)[32,31]

SE<-sqrt(fuck_sq*x^4+fuck_cov*2*x^3+fuck_main*x^2+
fuck_sq*x0^4+fuck_cov*2*x0^3+fuck_main*x0^2-
2*(fuck_sq*x^2*x0^2+fuck_main*x*x0+fuck_cov*(x^2*x0+x*x0^2)))

SE[SE<0]<-0

results<-cbind(EV,EV-1.96*SE,EV+1.96*SE)
 dev.new(width=5, height=2.5, unit="in")
png("multi-af_new.png")
matplot(x,results,type="l",lty=c(1,2,2),col=1, xlab=expression(italic("Player Rank")), ylab=expression(italic("Expected Utility of Gameplay")), lwd=1, axes=F)
axis(1, at=c(-0.4, -0.2, 0, 0.2, 0.4), lab=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2)
abline(h=0,col=2)
legend("topright", legend=c("Estimated Utility", "95% CI"), lty=1:2)
dev.off()

## What's the Effect of the Update on Gaming Behavior? 1
x<-seq(-0.5,0.5,length=1000)
EV<-as.numeric(cbind(x^2,x,1)%*%coef(m_multi2)[c(32,31,13)])
plot(x,EV,type="l",col=1)
abline(h=0,col=2)

SE<-sqrt(vcov(m_multi2)[32,32]*x^4+vcov(m_multi2)[31,31]*x^2+vcov(m_multi2)[13,13]+
2*(vcov(m_multi2)[32,31]*x^3+vcov(m_multi2)[32,13]*x^2+vcov(m_multi2)[31,13]*x))

results<-cbind(EV,EV-1.96*SE,EV+1.96*SE)
 dev.new(width=5, height=2.5, unit="in")
png("multi-effect_new.png")
matplot(x,results,type="l",lty=c(1,2,2),col=1, xlab=expression(italic("Player Rank")), ylab=expression(italic("Expected Utility of Gameplay")), lwd=1, axes=F)
axis(1, at=c(-0.4, -0.2, 0, 0.2, 0.4), lab=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2)
abline(h=0,col=2)
legend("topright", legend=c("Estimated Utility", "95% CI"), lty=1:2)
dev.off()
978-450+1#529


## Random Model Graph ##
str(summary(m_multi2_r))
length(summary(m_multi2_r)[[10]][1:41])
cbind(1:41,summary(m_multi2_r)[[10]][1:41]) ## What are "rtt2" and "game_time"? Why no interaction with udpate?


## Assessing significamce of "After" estimates
n1<-1:12
n2<-c(13,31:41)

est<-summary(m_multi2_r)[[10]][n1]+summary(m_multi2_r)[[10]][n2]
se0<-diag(vcov(m_multi2_r))[n1]+diag(vcov(m_multi2_r))[n2]
for(i in 1:12){
se0[i]<-se0[i]+2*vcov(m_multi2_r)[n1[i],n2[i]]
print(i)
}
se<-sqrt(se0)
t<-est/se
p<-2*(1-pt(abs(t),560519))

cbind(est,se,t,p)


## Social Status and Gaming Behavior 1: Before Updates
x<-seq(-0.5,0.5,length=1000)
EV0<-as.numeric(cbind(x^2,x)%*%coef(m_multi2_r)[3:2])
w0<-which.max(EV0)
x0<-x[w0]
a0<-EV0[w0]
EV<-as.numeric(cbind(x^2,x)%*%coef(m_multi2_r)[3:2])-a0
matplot(x,cbind(EV0,EV),type="l",lty=c(1,2,2),col=1)
abline(h=0,col=2)

SE<-sqrt(vcov(m_multi2_r)[3,3]*x^4+vcov(m_multi2_r)[3,2]*2*x^3+vcov(m_multi2_r)[2,2]*x^2+
vcov(m_multi2_r)[3,3]*x0^4+vcov(m_multi2_r)[3,2]*2*x0^3+vcov(m_multi2_r)[2,2]*x0^2-
2*(vcov(m_multi2_r)[3,3]*x^2*x0^2+vcov(m_multi2_r)[2,2]*x*x0+vcov(m_multi2_r)[3,2]*(x^2*x0+x*x0^2)))
 dev.new(width=5, height=2.5, unit="in")
 png("multi-bf2_r.png")
results<-cbind(EV,EV-1.96*SE,EV+1.96*SE)
matplot(x,results,type="l",lty=c(1,2,2),col=1, xlab=expression(italic("Player Rank")), ylab=expression(italic("Expected Utility of Gameplay")),lwd=1, axes=F)
axis(1, at=c(-0.4, -0.2, 0, 0.2, 0.4), lab=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2)
abline(h=0,col=2)
legend("bottomright", legend=c("Estimated Utility", "95% CI"), lty=1:2)
dev.off()


## Social Status and Gaming Behavior 2: After Updates
fuck<-coef(m_multi2_r)[32:31]+coef(m_multi2_r)[3:2]
EV0<-as.numeric(cbind(x^2,x)%*%fuck)
w0<-which.min(EV0)
x0<-x[w0]
a0<-EV0[w0]
EV<-as.numeric(cbind(x^2,x)%*%fuck)-a0
matplot(x,cbind(EV0,EV),type="l",lty=c(1,2,2),col=1)
abline(h=0,col=2)

fuck_sq<-vcov(m_multi2_r)[3,3]+vcov(m_multi2_r)[32,32]+2*vcov(m_multi2_r)[3,32]
fuck_main<-vcov(m_multi2_r)[2,2]+vcov(m_multi2_r)[31,31]+2*vcov(m_multi2_r)[2,31]
fuck_cov<-vcov(m_multi2_r)[3,2]+vcov(m_multi2_r)[3,31]+vcov(m_multi2_r)[32,2]+vcov(m_multi2_r)[32,31]

SE<-sqrt(fuck_sq*x^4+fuck_cov*2*x^3+fuck_main*x^2+
fuck_sq*x0^4+fuck_cov*2*x0^3+fuck_main*x0^2-
2*(fuck_sq*x^2*x0^2+fuck_main*x*x0+fuck_cov*(x^2*x0+x*x0^2)))

SE[SE<0]<-0

results<-cbind(EV,EV-1.96*SE,EV+1.96*SE)
 dev.new(width=6, height=2.5, unit="in")
png("multi-af_2_r.png")
matplot(x,results,type="l",lty=c(1,2,2),col=1, xlab=expression(italic("Player Rank")), ylab=expression(italic("Expected Utility of Gameplay")), lwd=1, axes=F)
axis(1, at=c(-0.4, -0.2, 0, 0.2, 0.4), lab=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2)
abline(h=0,col=2)
legend("topright", legend=c("Estimated Utility", "95% CI"), lty=1:2)
dev.off()



## What's the Effect of the Update on Gaming Behavior? 1
x<-seq(-0.5,0.5,length=1000)
EV<-as.numeric(cbind(x^2,x,1)%*%coef(m_multi2_r)[c(32,31,13)])
plot(x,EV,type="l",col=1)
abline(h=0,col=2)

SE<-sqrt(vcov(m_multi2_r)[32,32]*x^4+vcov(m_multi2_r)[31,31]*x^2+vcov(m_multi2_r)[13,13]+
2*(vcov(m_multi2_r)[32,31]*x^3+vcov(m_multi2_r)[32,13]*x^2+vcov(m_multi2_r)[31,13]*x))

results<-cbind(EV,EV-1.96*SE,EV+1.96*SE)
 dev.new(width=6, height=2.5, unit="in")
png("multi-af-bf2_r.png")
matplot(x,results,type="l",lty=c(1,2,2),col=1, xlab=expression(italic("Player Rank")), ylab=expression(italic("Expected Utility of Gameplay")), lwd=1, axes=F)
axis(1, at=c(-0.4, -0.2, 0, 0.2, 0.4), lab=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2)
abline(h=0,col=2)
legend("topright", legend=c("Estimated Utility", "95% CI"), lty=1:2)
dev.off()
978-450+1#529

