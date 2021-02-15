
library(parallel)
numCores<-parallel::datectCores()-1
myCluster<-parallel::makeCluster(numCores)
parallel::parLapply(cl=myCluster, x=2:4, fun=function(x){2^x})
parallel::stopCluster(myCluster)
parallel::parLapply(cl=myCluster, x=2:4, fun=function(x){2^x})


setwd("C:\\Users\\moonf\\Desktop")

iseq<-seq(1,10000,1)
parLapply(myCluster,iseq,function(y){write(y,"progress.txt",append=T)})

parallel::stopcluster(myCluster)

#변수 스코프
numCores<-parallel::datectCores()-1
myCluster<-parallel::makeCluster(numCores)

base<-2
parallel::clusterExport(myCluster,"base")
parallel::parLapply(cl=myCluster,x=2:4,
                    fun=function(x){base^x})
parallel::stopCluster(myCluster)

#foreach 
library(foreach)
library(doParallel)

numCores<-parallel::datectCores()-1
myCluster<-parallel::makeCluster(numCores)
doParallel::registerDoParallel(myCluster)


base<-2
parallel::clusterExport(myCluster,"base")

foreach::foreach(exponent=2:4, .combine=c) %dopar%{base^exponent}
parallel::stopCluster(myCluster)

test<-function(exponent){
  foreach::foreach(exponent=2:4,
                   .combine=c,
                   .export="base") %dopar%{
                     base^exponent
                   }
}



numCores<-parallel::datectCores()-1
myCluster<-parallel::makeCluster(numCores)
doParallel::registerDoParallel(myCluster)

set.seed(1234)
folds=createFolds(iris$Sepal.Length,k=3)
td_tmp=foreach::foreach(k=1:3,
                        .combine=rbind,
                        .packages=c("dplyr","broom","caret"),
                        .inorder=TRUE) %dopar%{
                          tmp=iris[-unlist(folds[k]),] %>% group_by(Species) %>%
                            do(fit=lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=.))
                          tidy(tmp.fit)
                          
                        }

parallel::stopCluster(myCluster)


numCores<-parallel::datectCores()-1
myCluster<-parallel::makeCluster(numCores)

foreach(x=list(1,2,"a")) %dopar% {
  tryCatch({
    c(1/x,x,2^x)
  },error=function(e){
    return(paste0("The variable",x,""," caused the error: ",e,""))
  })
}
parallel::stopCluster(myCluster)

system.time({for(i in 1:1000){i+5}})


n_core=detectCores()
cl=makeCluster(n_core-1)
registerDoParallel(cl)
system.time(
  {foreach(i=1:10000) %dopar%{
    i+5
  }}
)

####################h20 실습
library(dplyr)
library(caret)

flights_data=readRDS("flights.RDS")
head(flights_data)

str(flights_data)

flights_data$target<-ifelse(is.na(flights_data$dep_delay)|(flights_data$dep_delay<=30 & flights_data$dep_delay>=-30)) &
  (is.na(flights_data$arr_delay) | (flights_data$arr_delay<=30 & flights_data$arr_delay>=-30)),"normal","delay"

final_data<-flights_data %>% select("month", "carrier","flight","dest","air_time","distance",
                                    "target")
str(final_data)
final_data$carrier<-as.factor(final_data$carrier)
fianl_data$dest<-as.factor(final_data$dest)
final_data$target<-as.factor(final_data$target)

set.seed(1234)
train_idx<-createDataPartition(final_data$target,p=0.7,list=F)
train<-final_data[train_idx,]
test<-final_data[-train_idx,]

install.packages("h20")
library(h20)
Sys.setenv(JAVA_HOME="c:\\program Files\\Java\\jdk-13.0.2")
h20.init(nthreads=15,max_mem_size="10g")

train_data_h20<-as.h20(train,destination_frame="train_data_h20")
test_data_h20<-as.h20(test,destination_frame="test_data_h20")

target<-"target"
features<-name(train)[!names(train) %in% target]
target; features

rf_model<-h2o.randomForest(x=features,y=target,training_frame=train_data_h20,
                           model_id="rf_model",ntress=500,seed=1234,mtries=floor(ncol(train)/3),verbose=F)

test_predict<-h2o.predict(rf_model,newdata=test_data_h2o)
test_predict

h2o.confusionMatrix(rf_model,newdata=test_data_h2o)
h2o.confusionMatrix(rf_model,newdata=test_data_h2o,metrics="accuracy")
h2o.confusionMatrix(rf_model,newdata=test_data_h2o,metrics="accuracy",thresholds=0.5)

plot(rf_model)
h2o.varimp_plot(rf_model,num_of_features=6)



































































