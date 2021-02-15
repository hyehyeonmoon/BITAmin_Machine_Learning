
################   데이터 전처리   ##################


setwd("C:\\Users\\내문서\\Desktop\\비타민동아리\\6_1차.R_200228")

air = read.csv("./air_data_180325.csv", stringsAsFactors = F)

head(air)

str(air)

for(i in 4:9){
  
  air[,i] = as.numeric(air[,i])
  
}
sum(is.na(air))

air<-na.omit(air)

str(air)

air[,3]<-as.factor(air[,3])

##################iris와 비슷하게 만들어보기(대표적인 예제로서 사람들이 많이 이용해서 참고가 크게 됨+이해하기 쉬움) ###

library(dplyr)

air %>% group_by(cityName) %>% tally()

city_co_df = air %>% group_by(cityName) %>%
  
  summarize(mean_co = mean(coValue, na.rm = T))


city_co_df<-as.data.frame(city_co_df)

city_co_df[city_co_df[,2]==max(city_co_df[,2]),] ##종로구
city_co_df[city_co_df[,2]==median(city_co_df[,2]),] ##용산구(중앙값과 가까운 구)
city_co_df[city_co_df[,2]==min(city_co_df[,2]),] ##구로구


##용산구, 종로구, 구로구만 이루어져 있는 데이터 만들기

air2<-air %>% filter(cityName %in% c("용산구","종로구","구로구"))

head(air2)

str(air2)

##정규화
normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

air2[,4:9]<-as.data.frame(apply(air2[,4:9],2,normalize))



## 변수들 간의 관계 확인
x11()
pairs(air2[,4:9])

library(psych)
pairs.panels(air2[,4:9])  ##pm25Value(미세먼지), no2Value 비례관계/ pm10Value, pm25Value가 비례관계


##

air3<-air2 %>% select(cityName,no2Value, coValue)

head(air3)
str(air3)



###############   KNN   ##################3


##train, test 분류하기(sample 이용)
set.seed(123)
idx<-sample(x=c("train","test"),size=nrow(air3),replace=T,prob=c(7,3))
train<-air3[idx=="train",]
test<-air3[idx=="test",]

head(train)
head(test)

train_x<-train[,-1]
train_y<-train[,1]
test_x<-test[,-1]
test_y<-test[,1]

##k가 1일 때 knn

set.seed(123)

install.packages("class")
library(class)

knn_1<-knn(train=train_x,test=test_x,cl=train_y,k=1)

######그래프로 보는 정확성
install.packages("scales")
library(scales)

#factor 요소를 3개로 바꾸어주기(plot 그리려고)
train$cityName <- as.character(train$cityName)
train$cityName <- as.factor(train$cityName) 
test$cityName <- as.character(test$cityName)
test$cityName <- as.factor(test$cityName)

#정확성--그래프로 확인
plot(formula=no2Value*10~coValue,data=train, col=alpha(c("purple","blue","green"),0.7)[train$cityName],pch=20)
points(formula=no2Value*10~coValue,data=test, col=alpha(c("purple","blue","green"),0.7)[test$cityName],pch=5)


##정확성 확인
accuracy_1 <- sum(knn_1 == test_y) / length(test_y) ; accuracy_1

library(caret)
accuracy_caret <- confusionMatrix(knn_1,test_y)


##############적당한 k 값 찾기
# 분류 정확도 사전 할당
accuracy_k <- NULL

# kk가 1부터 train 행 수까지 증가할 때 (반복문)
for(kk in c(1:nrow(train_x))){
  
  # k가 kk일 때 knn 적용하기
  set.seed(1234)
  knn_k <- knn(train = train_x,
               test = test_x,
               cl = train_y,
               k = kk)
  
  # 분류 정확도 계산하기
  accuracy_k <- c(accuracy_k, confusionMatrix(knn_k,test_y)$overall[1])
}

# k에 따른 분류 정확도 데이터 생성
test_k <- data.frame(k = c(1:nrow(train_x)), accuracy = accuracy_k)

# k에 따른 분류 정확도 그래프 그리기
plot(formula = accuracy ~ k,
     data = test_k,
     type = "o",
     pch = 20,
     main = "validation - optimal k")

# 그래프에 k 라벨링 하기
with(test_k, text(accuracy ~ k, labels = rownames(test_k), pos = 1, cex = 0.7))

# 분류 정확도가 가장 높으면서 가장 작은 k는?
min(test_k[test_k$accuracy %in% max(accuracy_k), "k"])

###k=32로 나옴

##################복습문제 : pm10value, pm25value 이용해서 
##################test 해보고, 정확도 계산하고, 적절한 k 값 찾아보기



############  Kmeans ###########3


str(air2)

x11()
pairs(air2[,4:9])

pairs.panels(air2[,4:9])  
##pm25Value(미세먼지), no2Value 비례관계/ pm10Value, pm25Value가 비례관계

##다시 새롭게 불러와주어야 함(정규화한 것을 다시 표준화 하는 거 비추)

air3<-air2 %>% select(cityName,no2Value, coValue)

head(air3)
str(air3)

###train, test set으로 분류(caret 이용)
library(caret)

set.seed(123)

train_idx<-createDataPartition(y=air3$cityName, p=0.7, list=F)
train_kmeans<-air3[train_idx,]
test_kmeans<-air3[-train_idx,]

##표준화
#K-means 군집 분석은 관측치 간의 거리를 이용하기 때문에 
#변수의 단위가 결과에 큰 영향을 미칩니다. 
#그래서 변수를 표준화 하는 작업이 필요한데요, scale 함수를 사용해서 표준화를 합니다.

train_kmeans[,2:3]<-scale(train_kmeans[,2:3])
test_kmeans[,2:3]<-scale(test_kmeans[,2:3])
summary(train_kmeans)
summary(train_kmeans)

set.seed(123)

air_kmeans<-kmeans(train_kmeans[,-1],centers=3,iter.max=10000)
air_kmeans

air_kmeans$cluster #각 개체별 할당된 군집 번호
air_kmeans$centers ##군집의 중심 좌표 행렬
air_kmeans$size #각 군집의 개체 개수


##군집 확인
class(train_kmeans) ##ggplot은 dataframe 가능
class(air_kmeans$cluster)
train_kmeans$train_cluster<-as.factor(air_kmeans$cluster)

ggplot(train_kmeans) + geom_point(aes(x=no2Value,
                                      y=coValue,col=train_cluster,group=train_cluster))
table(train_kmeans$cityName, train_kmeans$train_cluster) ##오차가 있음을 확인 할 수 있음(완벽하지 않은 모형)


##training 데이터 셋을 사용해서 예측 모델을 만들고,
##testing 데이터 셋으로 모델의 정확성을 다시 한번 확인
install.packages("e1071")
library(e1071)
set.seed(123)

modFit<-train(x=train_kmeans[,c(-1,-4)],y=train_kmeans$train_cluster,method='rpart')
test_kmeans<-as.data.frame(test_kmeans)
testClusterPred<-predict(modFit, test_kmeans)
table( test_kmeans$cityName, testClusterPred) #오차가 있음을 확인 할 수 있음(완벽하지 않은 모델)


###elbow 기법 실습 준비#####################

train_kmeans<-air3[train_idx,]
train_kmeans[,2:3]<-scale(train_kmeans[,2:3])
train_elbow<-train_kmeans[,1:3]

###elbow 기법 실습#########################
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k_max <- 15
wss <- sapply(1:k_max, 
              function(k){kmeans(train_elbow[,-1], k, iter.max = 1000 )$tot.withinss})

#####sapply 설명
#sapply : 벡터, 리스트, 표현식, 데이터 프레임 등에 함수를 적용하고 그 결과를 벡터 또는 행렬로 반환한다.

#sapply(
#  X,    # 벡터, 리스트, 표현식 또는 데이터 프레임
#  FUN,  # 적용할 함수
#  ...,  # 추가 인자. 이 인자들은 FUN에 전달된다.
#)
#반환 값은 FUN의 결과가 길이 1인 벡터들이면 벡터, 길이가 1보다 큰 벡터들이면 행렬이다.

######tot.withinss 설명
#withinss : 응집도. 같은 클러스터 안에 얼마나 데이터가 뭉쳐져있는지. 작아야 좋습니다!
#  tot.withinss : withinss 모두 더한 값입니다.


wss
plot(1:k_max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
##k=3일 때 급격히 기울기가 완만해지는 것을 볼 수 있음


#######복습 문제 : pm25value, pm10value로 train, test set 만들고, kmeans 함수 이용해 군집분류, 모델링
#######복습 문제 : 1주차 때 배운 교차검증 for문 이용해서 최적의 모형 찾아내기











