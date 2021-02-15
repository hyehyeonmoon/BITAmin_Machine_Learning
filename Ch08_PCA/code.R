
library(HSAUR)
library(car)
data(heptathlon)
head(heptathlon)
str(heptathlon)

plot(heptathlon)
cor(heptathlon)

heptathlon$urdles<-max(heptathlon$hurdles)-heptathlon$hurdles
heptathlon$run200m <-max(heptathlon$run200m)-heptathlon$run200m
heptathlon$run800m<-max(heptathlon$run800m)-heptathlon$run800m
plot(heptathlon)

fit<-lm(score~., data=heptathlon)
summary(fit)
vif(fit)

pc.fift<-princomp(subset(heptathlon,select=-score),cor=T,scores=T)
pc.fit$scores

cor(pc.fit$scores)

pc.fit$loadings

summary(pc.fit)

screeplot(pc.fit, type="l",pch=19,main="screeplot")

bigplot(pc.fit, scale=F, cex=0.7)

print(secu_precomp)

new_data<-as.data.frame(cbind(pc.fit$scores[,1:2],heptathlon$score))
heaad(new_data)
colnames(new_data)[3]<-"score"


pc.lm<-lm(score~.,data=new_data)
summary(pc.lm)
summary(fit)$adj.r.squared
summary(pc.lm)$adj.r.squared
vif(pc.lm); cor(new_data[,1:2])

svd.fit<-precomp(subset(heptathlon,select=score),center=T,scale=T)
swd.fit$rotation
pc.fit$loadings
svd.fit$x

vars<-apply(svd.fit$x,2,var)
props<-vars/sum(vars)
props

cumsum(props)






