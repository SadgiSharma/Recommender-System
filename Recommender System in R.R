getwd()
setwd("E:/MBA BA3/SMWA")
getwd()

#Loading data
data<-read.csv("movielensdata.csv",header=FALSE)
View(data)

#Assign column names
colnames(data)<-c("user_id","item_id","rating","timestamp")
View(data)
data<-data[,-which(names(data) %in% c("timestamp"))]
str(data)
summary(data)

#Histogram
hist(data$rating)

#Install recommenderlab
install.packages("recommenderlab")
library(recommenderlab)
data<-as(data,"realRatingMatrix")
class(data)
View(data)
data<-data[rowCounts(data)>20, colCounts(data)>50]
nrow(data)
ncol(data)

#Split data into train and test set
set.seed(210)
samplespl<-sample(x=c(TRUE,FALSE), size=nrow(data),replace=TRUE,prob=c(0.7,0.3))
data_train<-data[samplespl,]
data_test<-data[!samplespl,]
dim(data_test)
dim(data_train)

#Create User Based RS
ubcfmodel<-Recommender(data_train, method="UBCF", parameter=list(k=25, method="Cosine"))
ubcfmodel
#Predict top 5 recommendations
ubcfpredict<-predict(ubcfmodel,data_test[115:116],n=5)
ubcfpredict
as(ubcfpredict,"list")
#Predict ratings of users
predictratingsubcf<-predict(ubcfmodel, data_test[115:116],type="ratings")
predictratingsubcf
as(predictratings, "matrix")[,1:10]


#Create Item Based RS
user=115
ibcfmodel<-Recommender(data_train, method="IBCF", parameter=list(k=25, method="Cosine"))
ibcfmodel
#Predict top 5 recommendations based on ibcf
ibcfpredict1<-predict(ibcfmodel,data_test[115:116], n=5)
ibcfpredict1
as(ibcfpredict1,"list")

user_ratings<-data_test[user]
as(user_ratings,"list")
#Predict ratings for users using ibcf
predictratingsibcf<-predict(ibcfmodel, data_test[115:116],type="ratings")
predictratingsibcf
as(predictratings, "matrix")[,1:10]


WITH EVALUATION:
  data2<-read.csv("movielensdata.csv",header=FALSE)
data2<-as(data2,"realRatingMatrix")
eval1 <- evaluationScheme(data2, method = "split", train = .7,
                          k = 1, given = 10, goodRating = 4)

algos <- list("user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                             method="Cosine",
                                                             nn=25, minRating=3)),
              "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score",method="Cosine",
                                                             nn=25, minRating=3))
              
)

# Predict next n movies
result <- evaluate(eval1, algos, n=c(4, 8, 10, 13, 23))

# Draw ROC curve
plot(result)
