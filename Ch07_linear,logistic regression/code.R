
#7주차 비타민 예습과제

setwd("C:\\Users\\moonf\\Desktop\\비타민동아리\\7주차")

#1.데이터 탐색단계

library(MASS)
library(caret)

data("Boston", package="MASS")
data<-Boston


str(data)

stem(data$medv)

i=which(data$medv==50)
boston<-ddata[-i,]
boston$chas<-factor(boston$chas)
boston$rad<-factor(boston$rad)

#2. 모델 훈련 단계 -단순선형회귀

m=lm(medv~rm, data=boston)
summary(m)

plot(boston$rm, boston$medv)
abline(m)

#3 예측
lm_result<-lm(medv~rm, data=boston)

#예측할 독립변수
room<-c(6,7,8,9,10)
df_input<-data.frame(rm=room)

#예측
predict_medv<-predict(lm_result, df_input, interval="confidence", level=0.95)

#결과
cbind(df_input, predict_medv)


#2. 모델 훈련단계 - 다중선형회귀

m2<-lm(medv~., data=boston)
summary(m2)

par(mfrow=c(2,2))
plot(m2)

m2.both<-step(m2, direction="both")
m2.both
summary(m2.both)

#다중공선성 확인하기
library(car)
vif(m2.slm)
summary(m1.slm)

#예측

pre_medv<-predict(m2.slm, boton, interval="confidence")
pre_medv<-as.data.frame(pre_medv)
pre_medv$actual<-boston$medv
head(pre_medv)
summary(pre_medv$actual)
with(pre_medv, sqrt(sum(actual-fit)^2)/nrow(boston)))

plot(pre_medv$fit, pre_medv$actual, xlim=c(0,50), ylim=c(0,50), xlab="predict", ylab="actual")
abline(a=0, b=1, col="red", lwd=3)


#############Multinomial Logistic Regression 실습

###data
president<-read.csv("president.csv", header=T)
dim(president)
str(president)

apply(president[,c(1,3,5,6)],2,unique)
    
###reference group(기준그룹)
levels(president$대선92)
president$대선92<-relevel(president$대선92, ref="클링턴")
president$성별<-relevel(president$성별, ref="여자")
president$학력<-relevel(president$학력, ref="대학원")

###multinom model 구축

library(nnet)
presi.multi<-multinom(대선92~학력+성별+학력:성별, data=president) #fullmodel
step.multi<-step(presi.multi, direction="both", trace=F) #단계선택법

multi.sum<-summary(step.multi)
multi.sum

###p-value 구하기

z<-multi.sum$coefficients/multi.sum$standard.errors
p<-round((1-pnorm(abs(x),0,1))*2,3)
p

###exp(b)값 구하기
round(exp(multi.sum$coefficients),3)

###moel 예측
head(fitted(step.multi)) #각 범주에 속할 확률

#predict(step.multi, president, type="probs")와 같은 방법
pred<-predict(step.multi, president)
confusionMatrix(pred, president$대선92)








