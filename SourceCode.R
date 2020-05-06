## Created by: Lora Milam
## Group: 12
## Course: CS 4331 003: Information Retrieval
##
##

## Import dataset from download directory 
winequality.white <- read.csv("C:/Users/default.LAPTOP-QPG8VPNC/Downloads/winequality-white.csv", sep=";")

##Use Regression Modeling to find relevant predictor variables
model01<-lm(formula=quality~alcohol+sulphates+pH+density+total.sulfur.dioxide+free.sulfur.dioxide+chlorides+residual.sugar+citric.acid+volatile.acidity+fixed.acidity,data = winequality.white)

##Model01 shows that Total.Sulfur.Dioxide,Chlorides, Citric.Acid variables have high P-values
##Remove these variables
winequality.white$total.sulfur.dioxide<-NULL
winequality.white$citric.acid<-NULL
winequality.white$chlorides<-NULL

##New model
model02<-lm(formula=quality~alcohol+sulphates+pH+density+free.sulfur.dioxide+residual.sugar+volatile.acidity+fixed.acidity,data = winequality.white)
summary(model02)

##After Preprocessing, create training and test set
set.seed(123)
n<-dim(winequality.white)[1]
divide_ind<-runif(n)<.75
wine_train<-winequality.white[divide_ind,]
wine_test<-winequality.white[!divide_ind,]

##Create models for training and set set
Model02_train<-lm(formula=quality~alcohol+sulphates+pH+density+free.sulfur.dioxide+residual.sugar+volatile.acidity+fixed.acidity,data = wine_train)
summary(Model02_train)
Model02_test<-lm(formula=quality~alcohol+sulphates+pH+density+free.sulfur.dioxide+residual.sugar+volatile.acidity+fixed.acidity,data = wine_test)
summary(Model02_test)

##Plot Regressive Model Vs Actual results
##Dataframe for predictive variables
X_train<-subset(wine_train,select = c("fixed.acidity","volatile.acidity","residual.sugar","free.sulfur.dioxide","density","pH","sulphates","alcohol"))
X_test<-subset(wine_test,select = c("fixed.acidity","volatile.acidity","residual.sugar","free.sulfur.dioxide","density","pH","sulphates","alcohol"))
##Predictive models for training and test sets
Model02_train_predict<-predict(object = Model02_train,newdata = X_train)
Model02_test_predict<-predict(object = Model02_test,newdata = X_test)
#Plot for predictive models
plot(round(Model02_train_predict))
plot(round(Model02_test_predict))
#Plot for Actual results
plot(wine_train$quality)
plot(wine_test$quality)

##Plotting Kmeans
##Install packages
install.packages('tidyverse')
install.packages('cluster')
install.packages('factoextra')
##Libraries
library(tidyverse)
library(cluster)
library(factoextra)
#Dataframe prep
wine_train<-scale(wine_train)
wine_test<-scale(wine_test)
#Plotting test and training set for Factor of 1
wine_train_kmeans<-kmeans(wine_train,centers = 1)
wine_test_kmeans<-kmeans(wine_test,centers = 1)
plot(winequality.white$quality, col = wine_train_kmeans$cluster)
plot(winequality.white$quality, col = wine_test_kmeans$cluster)
#Plotting test and training set for Factor of 2
wine_train_kmeans<-kmeans(wine_train,centers = 2)
wine_test_kmeans<-kmeans(wine_test,centers = 2)
plot(winequality.white$quality, col = wine_train_kmeans$cluster)
plot(winequality.white$quality, col = wine_test_kmeans$cluster)
#Plotting test and training set for Factor of 5
wine_train_kmeans<-kmeans(wine_train,centers = 5)
wine_test_kmeans<-kmeans(wine_test,centers = 5)
plot(winequality.white$quality, col = wine_train_kmeans$cluster)
plot(winequality.white$quality, col = wine_test_kmeans$cluster)
#Plotting test and training set for Factor of 10
wine_train_kmeans<-kmeans(wine_train,centers = 10)
wine_test_kmeans<-kmeans(wine_test,centers = 10)
plot(winequality.white$quality, col = wine_train_kmeans$cluster)
plot(winequality.white$quality, col = wine_test_kmeans$cluster)

##Plotting Hierarchical Clustering
##Plotting test and training set for Factor of 1 
d_train<-dist(wine_train)
d_test<-dist(wine_test)
hc1_train<-hclust(d_train,method = "ward.D")
hc1_test<-hclust(d_test,method = "ward.D")
plot(hc1_train)
plot(hc1_test)
##Factor of 2
plot(hc1_train)
rect.hclust(hc1_train,k=2,border = 2:11)
plot(hc1_test)
rect.hclust(hc1_test,k=2,border = 2:11)
##Factor of 5
plot(hc1_train)
rect.hclust(hc1_train,k=5,border = 2:11)
plot(hc1_test)
rect.hclust(hc1_test,k=5,border = 2:11)
##Factor of 10
plot(hc1_train)
rect.hclust(hc1_train,k=10,border = 2:11)
plot(hc1_test)
rect.hclust(hc1_test,k=10,border = 2:11)
