# EDA
# GeoLocation
rm(list=ls())
MeteoLand<-read.csv("D:/Meteorite_Landings.csv", na.strings = "", stringsAsFactors = FALSE)
head(MeteoLand,10)

lattitude<-MeteoLand$reclat
longtitude<-MeteoLand$reclong

plot(longtitude,lattitude,main="Distribution of Meteorites Landings", 
     xlab="Longitude", ylab="Latitude")


# clear the outlier
## MeteoLand$reclong[22947]
MeteoLand2 <- MeteoLand[-c(22947), ]

lattitude2<-MeteoLand2$reclat
longtitude2<-MeteoLand2$reclong


plot(longtitude2,lattitude,main="Distribution of Meteorites Landings", 
     xlab="Longitude", ylab="Latitude")


longtitude2<-longtitude2[longtitude2 != 0]
lattitude2<-lattitude2[lattitude != 0]
par(mfrow=c(1,2))
hist(longtitude2,breaks=100)
hist(lattitude2,breaks=100)



# Mass
type<-MeteoLand$nametype
type<-as.matrix(type)
Relict<-0
for(i in (1:length(type))){
  if(type[i]=="Relict"){
    Relict=Relict+1
    i=i+1
  }
}
Relict



relict_mass<-MeteoLand$mass[MeteoLand$nametype == "Relict"]
valid_mass<-MeteoLand$mass[MeteoLand$nametype == "Valid"]

tf_relict<-is.na(relict_mass)
relict_mass<-relict_mass[!tf_relict]
mean(relict_mass)


tf_valid<-is.na(valid_mass)
valid_mass<-valid_mass[!tf_valid]
mean(valid_mass)


diff<-mean(valid_mass)-mean(relict_mass)
div<-mean(valid_mass)/mean(relict_mass)


mass = as.numeric(MeteoLand$mass)
head(MeteoLand$mass[order(mass, decreasing = T)],10)

par(mfrow=c(1,1))
boxplot(mass, horizontal =T,xlab = "Mass", main = "Meteorite's mass")


tf_mass<-is.na(mass)
mass<-mass[!tf_mass]
g_count<-0
for(i in 1:length(mass)){
  if(mass[i]>1000){
    g_count<-g_count+1
    i=1+1
  }
}
g_count



# Models
library(rpart.plot)
library(rpart)

set.seed(12345)
# train and test dataset
train<-MeteoLand2[c(1:300),]
test<-MeteoLand2[c(301:600),]

matrix<-cbind(MeteoLand2[c(1:300),]$id,MeteoLand2[c(1:300),]$mass,train$reclat,train$reclong)
df<-as.data.frame(matrix,na.rm =TRUE)
names(df)<-c("id","mass","reclat","reclong")
# Decision Tree
dt_model = rpart(df$mass ~ df$reclat + df$reclong , data = df, method = "anova")
rpart.plot(dt_model,cex=0.8)

matrix2<-cbind(MeteoLand2[c(301:360),]$id,MeteoLand2[c(301:360),]$mass,test$reclat,test$reclong)
df2<-as.data.frame(matrix2,na.rm =TRUE)
names(df2)<-c("id","mass","reclat","reclong")
dt_predictions = predict(dt_model, df2)

#Creating dataframe with actual and predicted values
df_pred = data.frame("actual"=df2, "dt_pred"=dt_predictions)
head(df_pred)


plot(df2$reclat,col="green")
plot(df2$reclong,col="green")
lines(dt_predictions,col="red")





# RandomForest
library(randomForest)
df<-na.omit(df)
rf_model = randomForest(df$id ~ df$mass + df$reclat + df$reclong, data = df, ntree=100)


rf_predictions = predict(rf_model, df2)
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions)




# K-means
library(ggplot2)
train<-MeteoLand2[c(1:10000),]
train<-na.omit(train)
sapply(train[,c(5,8,9)], var)

#ggplot(train,aes(x = reclat, y = reclong, col= "geolocation")) + geom_point()
#ggplot(train,aes(x = mass, y = reclat, col= "mReclat")) + geom_point()
#ggplot(train,aes(x = mass, y = reclong, col= "mReclong")) + geom_point()

k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(train[,8:9],k,nstart = 20,iter.max = 20)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(train[,8:9],3,nstart = 20)
icluster
table(icluster$cluster)

