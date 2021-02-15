rm(list=ls())
install.packages("dplyr")
install.packages("psych")
install.packages("car")
install.packages("randomForest")
library(dplyr)
library(psych)
library(car)
library(randomForest)

x11()

data<-read.csv("C:\\Users\\내문서\\Desktop\\비타민동아리\\insurance2.csv")
#데이터를 불러오면 구조랑 각종 값들 먼저 확인
str(data)
#범주형 변수 factor로 변경
data[ , c("sex","smoker","region")] = lapply(data[ , c("sex","smoker","region")], factor)

summary(data)
head(data)

#결측치처리-평균
hist(data$bmi)
data$bmi[is.na(data$bmi)]<-mean(data$bmi,na.rm=T)

#상당히 skewed되어 있음, 이상치로 볼것이냐?
hist(data$charges)

#범주형 변수들 확인
table(data$sex) ; table(data$smoker) ; table(data$region)
with(data,boxplot(charges~sex))
with(data,boxplot(charges~smoker))
with(data,boxplot(charges~region))

#수치형 변수들의 상관관계 보기
pairs(data[,unlist(lapply(data,is.numeric))])
pairs.panels(data[,unlist(lapply(data,is.numeric))]) #이쁘게 보여주기 위한 함수


data$bmi30<-as.factor(ifelse(data$bmi>=30,"비만","정상"))  ##bmi 30 이상이면 비만이라 함

boxplot(data$charges~data$bmi30) #비만인 사람이 비용이 좀 더 많아보이긴 한다
boxplot(data$charges~data$bmi30*data$smoker) # 흡연도 고려해본다
#데이터분할 랜덤하게 6:2:2
set.seed(123)
index<-sample(c("train","valid","test"),size=nrow(data),replace=T,prob=c(0.6, 0.2, 0.2))
table(index)

train<-data[index=="train",]
valid<-data[index=="valid",]
test<-data[index=="test",] ##carrot 이란 패키지가 랜덤분할 해주는 거 있음

#범주형 변수 reduced rank 방식 모델매트릭스  but, 알앙서 해준다~
##더미화--reduced rank
model.matrix(~., data = train)

#원래변수 회귀모형, fiiting
#갖고있는 변수를 회귀모형에 다 때려넣음
fit<-lm(charges~age+sex+bmi+children+smoker+region,data=train)
summary(fit)
#잔차 median이 -999인 것으로 보아 등분산 가정 안맞을 가정이 농후함


par(mfrow=c(2,2))
plot(fit)
summary(train[train$charges<=20000,]) ;summary(train[train$charges>20000,]) #?븵?꽌 ?궛?젏?룄 ?솗?씤?븳寃껋쿂?읆 媛덈┝

par(mfrow=c(1,2))
hist(data$charges) ; hist(log(data$charges)) 

#로그변환 및 bmi와 흡연 interaction 추가
fit2<-lm(log(charges)~age+sex+children+region+bmi30*smoker,data=train)
summary(fit2)
#median이 0에 가까워지는 것을 알 수 있음

par(mfrow=c(2,2))
plot(fit2)

vif(fit2)
Anova(fit2,type=3) ##변수 중요도를 보는 통계적 기법
##상호작용이 별 3개짜리로 잘 상호작용하면 유의하지 않더라도 넣어줘야 함

#평가지표 rmse, 별로 좋지 않은 모델...
rmse<-sqrt(sum((exp(predict(fit2,newdata=train))-train$charges)^2)/fit2$df.residual)
rmse ; summary(train$charges)

#validation에 rmse가 더 작은 이유는 train에 있던 이상치가 없어서인 것 같음
##원래 rmse가 더 안 좋은게 정상임
valid_rmse<-sqrt(sum((exp(predict(fit2,newdata=valid))-valid$charges)^2)/(nrow(valid)-fit2$rank))
valid_rmse ; summary(valid$charges) ; summary(train$charges)

par(mfrow=c(1,1))
plot(predict(fit2,newdata=valid),log(valid$charges))
plot(exp(predict(fit2,newdata=valid)),valid$charges)
#x=y직선에 근접해있지 않음.
abline(a=0,b=1,col="red")     

#랜덤포레스트
set.seed(123)
rf.fit<-randomForest(charges~age+sex+children+smoker+region+bmi30*smoker,data=train)
rf.fit ##MSE값 나옴
sqrt(26970068)

rf.pre<-predict(rf.fit,newdata=valid)
#휘귀분석이 아니면 n으로 나눔
rf.rmse<-sqrt(sum((rf.pre-valid$charges)^2)/nrow(valid))
rf.rmse
plot(rf.pre,valid$charges)
abline(a=0,b=1,col="red")     

#랜덤포레스트로 최종모형 결정
nrow(test)
final_pre<-predict(rf.fit,newdata=test)
sqrt(sum((final_pre-test$charges)^2)/nrow(test))
plot(final_pre,test$charges)
abline(a=0,b=1,col="red")     
