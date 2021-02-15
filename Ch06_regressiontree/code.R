
#Example : Predicting price of real estate(using regression and model tree)
##Step 1 : exploring and Splitting the data

setwd("C:\Users\moonf\Desktop\비타민동아리\6주차")
apt <- read.csv("Daegu_Real_Estate_data.csv")
colnames(apt)<-tolower(apt)
str(apt)
summary(apt)

options("scipen"=100, "digits"=10) #change exponential notation to fixed notation
# format(apt$SalePrice, scientific=F) : limmited local var
hist(apt$saleprice)

#splitting the data into train and test set
library(caret)

set.seed(408)

train_idx<-createDataPartition(apt$saleprice, p=.7, list=F)
apt_train<-apt[train_idx,]
apt_test<-apt[-train_idx,]

hist(apt_train$saleprice)
hist(apt_test$saleprice)

##step2 : Training a model on the data
#Regression tree using rpart
library(rpart)

set.seed(408)

apt_reg.tr <- rpar(saleprice~., data=apt_train)
apt_reg.tr
summary(apt_reg.tr)

plot(apt_reg.tr); text(apt_reg.tr)

apt_reg.tr
summary(apt_reg.tr)

#Visualization for decision tree
library(rpart.plot) #ref) "http://www.milbo.org/rpart-plot/"

rpart.plot(apt_reg.tr, digits=2)
rpart.plot(apt_reg.tr, digits=3, fallen.leaves=F, type=4, extra=101)


##Step 3 : Pruning the tree(for avoid overfitting)
printcp(apt_reg.tr)
plotcp(apt_reg.tr)
apt_reg_prune<-prune.rpart(apt_reg.tr, cp=apt_reg.tr$cptable[8, "cp"])

rpart.plot(apt_reg_prune, digits=3, fallen.leaves=F, type=4, extra=101)


##Step 4 : Evaluating model performance (using MAE)
apt_reg_pred<-predict(aapt_reg.tr, apt_test)
summary(apt_reg_pred)
summary(apt_test$saleprice)

cor(apt_reg_pred, apt_test$saleprice)

#function to caculate the mean absolute error(MAE)
MAE<-function(actual, predicted){
  mean(abs(actual-predicted))
}

MAE(apt_reg_pred, apt_test$saleprice)

#Comparing orig.regresiion tree with pruned tree & training data
apt_reg_prune_pred<-predict(apt_reg_prune, apt_test)

cor(apt_reg_prune_pred, apt_test$saleprice)
MAE(apt_reg_prune_pred, apt_test$saleprice)

mean(apt_train$saleprice) #result=221280
MAE(221280, apt_test$saleprice)

summary(apt_mod.tr)
apt_reg.tr

#Evaluation model tree
apt_mod_pred<-predict(apt_mod.tr, apt_test)
summart(apt_mod_pred)
summart(ap_test$saleprice)

cor(apt_mod_pred, apt_test$slaeprice)
MAE(apt_mod_pred, apt_test$saleprice)

## + Rpart in caret package
set.seed(408)

apt_rpart<-train(saleprice~., method="rpart", data=apt_train)
apt_rpart

plot(apt_rpart)

plot(apt_rpart$finalModel, uniform=T, main="Regression Tree")
text(apt_rpart$finalModel, use.n=T, all=T, cex=0.8)

apt_rpart

#Visualization for decision tree
library(rattle)

fancyRaprtPlot(apt_rpart$finalModel)

#Evaluation model performance
apt_rpart_pred<-predict(apt_rpart, apt_test)
cor(apt_rpart_pred, apt_test$saleprice)
MAE(apt_rpart_pred, apt_test$saleprice)






























