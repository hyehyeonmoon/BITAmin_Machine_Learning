
########실습1-iris data

library(adabag)
data("iris")
str(iris) #관측치 150개

set.seed(300)
train<-createDataPartition(iris$Species, p=0.7, list=F)

iris_ada<-boosting(Species~., data=iris[train,],mfinal=10,
                   control=rpart.control(maxdepth=1))
150*0.7
1/105
#자세히 살펴보기
print(names(iris_ada))
summary(iris_ada)

iris_ada$trees[1]
iris_ada$trees[7]
iris_ada$weights


result<-data.frame(iris_ada$class, iris_ada$votes, iris_ada$prob)
result


iris_ada$importance

importanceplot(iris_ada)

#예측하기
p_iris_ada<-predict(iris_ada, iris[test,])

p_iris_ada$confusion
p_iris_ada$error



# errorval

evol.train<-errorevol(iris_ada,newdata=iris[train,])
plot(evol.train)
abline(h=min(evol.train[[1]]), col="blue",lty=2,lwd=2)

evol.test<-errorevol(iris_ada,newdata=iris[-train,])
plot.errorevol(evol.test,evol.train)
abline(h=min(evol.test[[1]]), col="red",lty=2,lwd=2)



# Margin에 대해 알아보기

#training set margin
iris.margins<-margins(iris_ada,iris[train,])# training set
plot.margins(iris.margins)

# test set margin
iris.predmargins<-margins(p_iris_ada,iris[-train,])

#plot 그려보기
plot.margins(iris.predmargins,iris.margins)


###############################[참고코드]
# Margin에 대해 알아보기

library(rpart)
data(iris)
set.seed(300)
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

#training set margin
iris.adaboost <- boosting(Species ~ ., data=iris[train,], mfinal=3)
iris.margins<-margins(iris.adaboost,iris[train,])# training set
plot.margins(iris.margins)

# test set margin
iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-train,])
iris.predmargins<-margins(iris.predboosting,iris[-sub,])

#plot 그려보기
plot.margins(iris.predmargins,iris.margins)

###############################


###########실습2

setwd("C:\\Users\\moonf\\Desktop\\비타민동아리\\10주차")
credit<-read.csv("credit.csv")
str(credit)


##############실습2 - credit data

#adaboosting
set.seed(300)
credit_ada<-boosting(default~., data=credit)

#자세히 살펴보기
summary(credit_ada)

#rpart.control을 해주지 않았을 때
credit_ada$trees[1]

#변수 중요도
credit_ada$importance

#예측하기
p_credit_ada<-predict(credit_ada, credit)
p_credit_ada$confusion #과적합 발생
p_credit_ada$error


#교차검증을 통해 과적합 방지
set.seed(300)
cv_ada<-boosting.cv(default~., data=credit) #v : cv 수 조정(=k)
summary(cv_ada)

cv_ada$confusion
cv_ada$error


#회귀트리, 랜덤포레스트, 베깅 MSE에 대해서 비교

train<-sample(1:nrow(credit),nrow(credit)*0.7)
cr_train<-credit[train,]
cr_test<-credit[-train,]


#Regression tree 
library(rpart)
set.seed(300)
reg.tr<-rpart(default~., data=cr_train,method="class")
reg.tr_pred<-predict(reg.tr, cr_test, type="class")
confusionMatrix(cr_test$default, reg.tr_pred)

#Random Forest 
library(randomForest)
set.seed(300)
ranf.tr<-randomForest(default~., data=cr_train)
ranf.tr_pred<-predict(ranf.tr, cr_test, type="class")
confusionMatrix(cr_test$default, ranf.tr_pred)

#Bagging 
set.seed(300)
bg.tr<-randomForest(default~., data=cr_train, mtry=16)
bg.tr_pred<-predict(bg.tr, cr_test, type="class")
confusionMatrix(cr_test$default, bg.tr_pred)

#Adaboosting 
set.seed(300)
ada.tr<-boosting(default~., data=cr_train)
ada.tr_pred<-predict(ada.tr, cr_test) #cv로 하고 싶으나 boosting.cv 와 predict이 호환되지 않아 boosting 함수 이용
a<-as.factor(ada.tr_pred$class)
confusionMatrix(cr_test$default, a)


#################################iris에 대해 비교하는 건데 버림

#회귀트리, 랜덤포레스트, 베깅 ConfusionMatrix 비교

train<-sample(1:nrow(iris), nrow(iris)*0.7)
cr_train<-iris[train,]
cr_test<-iris[-train,]

#Regression tree
library(rpart)
set.seed(300)
reg.tr<-rpart(Species~., data=cr_train)
reg.tr_pred<-predict(reg.tr, cr_test, type="class")
confusionMatrix(cr_test$Species, reg.tr_pred)

#Random Forest
library(randomForest)
ranf.tr<-randomForest(Species~., data=cr_train)
ranf.tr_pred<-predict(ranf.tr, cr_test, type="class")
confusionMatrix(cr_test$Species, ranf.tr_pred)

#Bagging
bg.tr<-randomForest(Species~., data=cr_train, mtry=16)
bg.tr_pred<-predict(bg.tr, cr_test)
confusionMatrix(cr_test$Species, bg.tr_pred)

#Adaboosting
ada.tr<-boosting(Species~., data=cr_train)
ada.tr_pred<-predict(ada.tr, cr_test)
a<-as.factor(ada.tr_pred$class)
confusionMatrix(cr_test$Species, a)




















