# ExploratoryFactorAnalysis
rm(list=ls())
data <- read.csv("C:/Users/Jinlin Meng/Desktop/Data Analytics/dataset_EFA.csv")

data

install.packages("psych")
library(psych)

corMat  <- cor(data)

solution <- fa(r = corMat, nfactors = 2, rotate = "oblimin", fm = "pa")



# Principal Components and Factor Analysis
rm(list=ls())
fit <- princomp(mydata, cor=TRUE)
summary(fit) 
loadings(fit)
plot(fit,type="lines")
fit$scores
biplot(fit)



library(psych)
fit <- principal(mydata, nfactors=5, rotate="varimax")
fit


fit <- factanal(mydata, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

load <- fit$loadings[,1:2]
plot(load,type="n")
text(load,labels=names(mydata),cex=.7) 



library(psych)
fit <- factor.pa(mydata, nfactors=3, rotation="varimax")
fit


# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(mydata))
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)



# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(mydata)





# Titannic
data("Titanic")
titanic <- read.csv("C:/Users/Jinlin Meng/Desktop/Data Analytics/titanic_clean.csv")

str(titanic)
tail(titanic)
titanic <- titanic[-1310,]

library(ggplot2)
ggplot(titanic,aes(x=factor(pclass),fill=factor(sex)))+
  geom_bar(position="dodge")


ggplot(titanic,aes(x=factor(pclass),fill=factor(sex)))+
  geom_bar(position="dodge")+
  facet_grid(". ~ survived")



#install.packages('rpart')
library(rpart)

my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")



#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(my_tree_two)


all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])
train <- all_data[1:891,]
test <- all_data[892:1309,]
library(randomForest)
str(train)
str(test)

my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, importance=TRUE,ntree=1000)
varImpPlot(my_forest)
