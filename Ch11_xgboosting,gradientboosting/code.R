

library(xgboost)
data(iris)
head(iris)
str(iris)

species=iris$Species
label=as.integer(iris$Species)-1
iris$Species=NULL

set.seed(113)
n=nrow(iris)
train.index=sample(n, floor(0.7*n))
train.data=as.matrix(iris[train.index,])
train.label=label[train.index]
test.data=as.matrix(iris[-train.index,])
test.label=label[-train.index]
length(train.label)
length(test.label)

#Transform the two data sets into xggb.Matrix
xgb.train=xgb.DMatrix(data=tain.data,label=train.label)
xgb.test=xgb.DMatrix(data=test.data,label=test.label)
xgb.train
xgb.test

data$y<-as.numeric(data$y)-1
head(data$y)

x<-model.matrix(~., data=data%>% select(-y))
x<-as.matrix(x[,-1])
x<-as.data.frame(x)
train_mat<-as.matrix(x[,-1])
xgb.atrix<-xgb.DMatrix(train_mat, label=data$y)
xgb.matrix


#Define the parameters for multinomial classification
num_class=length(levels(species))
num_class
params=list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

#Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=2500,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
  #verbose=1로 두면 each round마다 결과를 확인 할 수 있다.
)

#Review the final model and results
xgb.fit

#Predict outcomes with the test data
xgb.pred=predict(xgb.fit, test.data, reshape=T)
xgb.pred=as.data.frame(xgb.pred)
xgb.pred

colnames(xgb.pred)<-c("setosa","versicolor","virginica")
xgb.pred


#Use the predicted label with the highest probability
xgb.pred$prediction=appl(xgb.pred,1,function(x)colnames(xgb.pred)[which.max(x)])
xgb.pred$prdiction<-as.factor(xgb.pred$prediction)
xgb.pred$prediction

xgb.pred$label=levels(species)[test.label+1]
xgb.pred$label<-as.factor(xgb.pred$label)

head(xgb.pred)

confusionMatrix(xgb.pred$prediction, xgb.pred$label)

var_mat<-xgb.importance(feature_names=colnames(test.data),model=xgb.fit)
xgb.plot.importance(importance_matrix=var_mat)





```{r}
parm<-expand.grid(max_depth=1:6, nrounds=seq(100,310,30))

for(i in 1:dim(parm)[1])
  params=list(booster="gbtree",
              eta=0.01,
              max_depth=parm[i,"max_depth"],
              gamma=3,
              subsample=0.8,
              objective="binary:logistic",
              eval_metric="error")

xgb.fit<-xgb.train(
  params=params,
  data=xg.tr,
  nrounds=parm[i,"nrounds"],
  watchlist=list(val1=xg.train,val2=xg.test),
  verbose=0
)

xgb.pred<-predict(xgb.fit, x_te, reshape=T)
xgb.pred<-as.data.frame(xgb.pred)

xgb.pred$prediction=apply(xgb.pred,1,function(x)colnames(xgb.pred)[which.max(x)])
xgb.pred$prdiction<-as.factor(xgb.pred$prediction)



confusionMatrix(xgb.pred$prediction, y_te)




```










