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

data<-read.csv("C:\\Users\\������\\Desktop\\��Ÿ�ε��Ƹ�\\insurance2.csv")
#�����͸� �ҷ����� ������ ���� ���� ���� Ȯ��
str(data)
#������ ���� factor�� ����
data[ , c("sex","smoker","region")] = lapply(data[ , c("sex","smoker","region")], factor)

summary(data)
head(data)

#����ġó��-���
hist(data$bmi)
data$bmi[is.na(data$bmi)]<-mean(data$bmi,na.rm=T)

#����� skewed�Ǿ� ����, �̻�ġ�� �����̳�?
hist(data$charges)

#������ ������ Ȯ��
table(data$sex) ; table(data$smoker) ; table(data$region)
with(data,boxplot(charges~sex))
with(data,boxplot(charges~smoker))
with(data,boxplot(charges~region))

#��ġ�� �������� ������� ����
pairs(data[,unlist(lapply(data,is.numeric))])
pairs.panels(data[,unlist(lapply(data,is.numeric))]) #�̻ڰ� �����ֱ� ���� �Լ�


data$bmi30<-as.factor(ifelse(data$bmi>=30,"��","����"))  ##bmi 30 �̻��̸� ���̶� ��

boxplot(data$charges~data$bmi30) #���� ����� ����� �� �� ���ƺ��̱� �Ѵ�
boxplot(data$charges~data$bmi30*data$smoker) # ������ �����غ���
#�����ͺ��� �����ϰ� 6:2:2
set.seed(123)
index<-sample(c("train","valid","test"),size=nrow(data),replace=T,prob=c(0.6, 0.2, 0.2))
table(index)

train<-data[index=="train",]
valid<-data[index=="valid",]
test<-data[index=="test",] ##carrot �̶� ��Ű���� �������� ���ִ� �� ����

#������ ���� reduced rank ��� �𵨸�Ʈ����  but, �˾Ӽ� ���ش�~
##����ȭ--reduced rank
model.matrix(~., data = train)

#�������� ȸ�͸���, fiiting
#�����ִ� ������ ȸ�͸����� �� ��������
fit<-lm(charges~age+sex+bmi+children+smoker+region,data=train)
summary(fit)
#���� median�� -999�� ������ ���� ��л� ���� �ȸ��� ������ ������


par(mfrow=c(2,2))
plot(fit)
summary(train[train$charges<=20000,]) ;summary(train[train$charges>20000,]) #?��?�� ?��?��?�� ?��?��?��것처?�� 갈림

par(mfrow=c(1,2))
hist(data$charges) ; hist(log(data$charges)) 

#�α׺�ȯ �� bmi�� ���� interaction �߰�
fit2<-lm(log(charges)~age+sex+children+region+bmi30*smoker,data=train)
summary(fit2)
#median�� 0�� ��������� ���� �� �� ����

par(mfrow=c(2,2))
plot(fit2)

vif(fit2)
Anova(fit2,type=3) ##���� �߿䵵�� ���� ����� ���
##��ȣ�ۿ��� �� 3��¥���� �� ��ȣ�ۿ��ϸ� �������� �ʴ��� �־���� ��

#����ǥ rmse, ���� ���� ���� ��...
rmse<-sqrt(sum((exp(predict(fit2,newdata=train))-train$charges)^2)/fit2$df.residual)
rmse ; summary(train$charges)

#validation�� rmse�� �� ���� ������ train�� �ִ� �̻�ġ�� ����� �� ����
##���� rmse�� �� �� ������ ������
valid_rmse<-sqrt(sum((exp(predict(fit2,newdata=valid))-valid$charges)^2)/(nrow(valid)-fit2$rank))
valid_rmse ; summary(valid$charges) ; summary(train$charges)

par(mfrow=c(1,1))
plot(predict(fit2,newdata=valid),log(valid$charges))
plot(exp(predict(fit2,newdata=valid)),valid$charges)
#x=y������ ���������� ����.
abline(a=0,b=1,col="red")     

#����������Ʈ
set.seed(123)
rf.fit<-randomForest(charges~age+sex+children+smoker+region+bmi30*smoker,data=train)
rf.fit ##MSE�� ����
sqrt(26970068)

rf.pre<-predict(rf.fit,newdata=valid)
#�ֱͺм��� �ƴϸ� n���� ����
rf.rmse<-sqrt(sum((rf.pre-valid$charges)^2)/nrow(valid))
rf.rmse
plot(rf.pre,valid$charges)
abline(a=0,b=1,col="red")     

#����������Ʈ�� �������� ����
nrow(test)
final_pre<-predict(rf.fit,newdata=test)
sqrt(sum((final_pre-test$charges)^2)/nrow(test))
plot(final_pre,test$charges)
abline(a=0,b=1,col="red")     