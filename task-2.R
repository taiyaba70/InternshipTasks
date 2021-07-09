rm(list=ls())

#Data Science and Business Analytics Internship
#Task-2
#Prediction using Unsupervised ML

#Packages which is useful for prediction the optimum number of clusters
#To install the packages
pkgs<-c("factoextra","NbClust","ggfortify","ggplot2","permute","lattice","vegan","dplyr")
install.packages(pkgs)

#To Load the packages
library(stats)
library(factoextra)
library(NbClust)
library(ggfortify)
library(ggplot2)
library(permute)
library(lattice)
library(vegan)
library(dplyr)

#Reading datafile from working directory

data<-read.csv("C:\\Users\\Taiyaba\\Desktop\\TSF\\iris.csv")
attach(data)

#Getting the view of Data file

View(data)

#Internal Structure of Dataset

str(data)

#Summary of given dataset

summary(data)

mydata<-select(data,c(1,2,3,4))


#Scatterplot for visualization before clustering

scatter<-ggplot(data,aes(x=Petal.Length,y=Petal.Width))+geom_point(aes(color=Species,shape=Species))
scatter<-scatter+theme_minimal()+ xlab("Petal Length")+ylab("Petal Width")
scatter<-scatter+ggtitle("Petal Length-width")+ theme(plot.title=element_text(hjust=0.5))
scatter

#Method-1

#within group sum of square to determine the optimal number of Clusters
#also called "ELBOW" Method
#As the number of clusters increases,the value of WSS decreases.
wss<-(nrow(mydata)-1)*sum(apply(mydata,2,var))
i=2
while(i<=15){
wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
i=i+1
}
plot(1:15,wss,type="b",xlab="Number of clusters",ylab="Within group sum of squares",col="blue")

#Method-2

#NbClust Function for determining the optimal number of Clusters

par(mar=c(2,2,2,2))
Idata<-iris[,1:4]
totalwss<-c()
NB<-NbClust(Idata,method="kmeans")

#histogram approach denoting various indices with different number of clusters voted

hist(NB$Best.nc[1,],breaks=15,main="Number of Clusters",col=c("red","green","blue","yellow"))

#Method-3
#Calinski-Harabasz index
#Also called the variance ratio criterian

modeldata<-cascadeKM(Idata,1,15,iter=100)
plot(modeldata,sortg=TRUE)
modeldata$results[2,]

#to determine which of these values is maximum.
which.max(modeldata$results[2,])










