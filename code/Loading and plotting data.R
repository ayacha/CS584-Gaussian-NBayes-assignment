setwd("/Users/ayacha/Desktop/cs584-assignment2")

data<-read.table("iris.data.txt", sep=',')

X=as.matrix(data[,1:4])
Y=as.matrix(data[,5])

# trellis plot
pairs(data[1:4],main="Iris Data", pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

