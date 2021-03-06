setwd("/Users/ayacha/Desktop/cs584-assignment2")

data<-read.table("iris.data.txt", sep=',')

dim(data)

test_idx <- seq(41, 60)
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]


x = trainingSet[-5]
xtest = testingSet[-5]
y = trainingSet[5]

sss = unique(y)

m1=sum(y=='Iris-setosa')
muu1 = aggregate(x, by=list(trainingSet$V5=='Iris-setosa'), sum)

mu1 = (1/m1)* c(muu1[2,2],muu1[2,3],muu1[2,4],muu1[2,5])

siig1 = matrix(0,4, 4)
seq = dim(x)[1]
for(i in 1:seq) {
    if(y[i,]=='Iris-setosa'){
      siig1 = siig1 + t(as.matrix(x[i,]-mu1))%*%as.matrix(x[i,]-mu1)
    }
}
sig1 = (1/m1)*(siig1)


m2=sum(y=='Iris-versicolor')
muu2 = aggregate(x, by=list(trainingSet$V5=='Iris-versicolor'), sum)

mu2 = (1/m2)* c(muu2[2,2],muu2[2,3],muu2[2,4],muu2[2,5])

siig2 = matrix(0,4, 4)
seq = dim(x)[1]
for(i in 1:seq) {
  if(y[i,]=='Iris-versicolor'){
    siig2 = siig2 + t(as.matrix(x[i,]-mu2))%*%as.matrix(x[i,]-mu2)
  }
}
sig2 = (1/m2)*(siig2)

m3=sum(y=='Iris-virginica')
muu3 = aggregate(x, by=list(trainingSet$V5=='Iris-virginica'), sum)

mu3 = (1/m3)* c(muu3[2,2],muu3[2,3],muu3[2,4],muu3[2,5])

siig3 = matrix(0,4, 4)
seq = dim(x)[1]
for(i in 1:seq) {
  if(y[i,]=='Iris-virginica'){
    siig3 = siig3 + t(as.matrix(x[i,]-mu3))%*%as.matrix(x[i,]-mu3)
  }
}
sig3 = (1/m3)*(siig3)



testYpredictions1 = c()
seq = dim(xtest)[1]
for(i in 1:seq) {
  testYpredictions1 = c(testYpredictions1,  (1/det(sig1))* exp(-0.5* as.matrix(xtest[i,]-mu1) %*% solve(sig1) %*% t(xtest[i,]-mu1) ) )  
}

testYpredictions2 = c()
seq = dim(xtest)[1]
for(i in 1:seq) {
  testYpredictions2 = c(testYpredictions2,  (1/det(sig2))* exp(-0.5* as.matrix(xtest[i,]-mu2) %*% solve(sig2) %*% t(xtest[i,]-mu2) ) )  
}

testYpredictions3 = c()
seq = dim(xtest)[1]
for(i in 1:seq) {
  testYpredictions3 = c(testYpredictions3,  (1/det(sig3))* exp(-0.5* as.matrix(xtest[i,]-mu3) %*% solve(sig3) %*% t(xtest[i,]-mu3) ) )  
}



yhat = c()
seq = dim(xtest)[1]
for(i in 1:seq) {
  if((testYpredictions1[i] > testYpredictions2[i]) &&(testYpredictions1[i] > testYpredictions3[i])){
    yhat = c(yhat, "Iris-setosa")  
  }
  else if ((testYpredictions2[i] > testYpredictions3[i]) &&(testYpredictions2[i] > testYpredictions1[i])) {
    yhat = c(yhat, "Iris-versicolor")  
  }
  else {
    yhat = c(yhat,  "Iris-virginica")  
  }
}


#yhat <- ifelse(((testYpredictions1 > testYpredictions2) &&(testYpredictions1 > testYpredictions3)), ifelse(((testYpredictions2 > testYpredictions3) &&(testYpredictions2 > testYpredictions1)), "Iris-setosa", "Iris-versicolor"), "Iris-virginica")

confusionMatrix=table(yhat, testingSet$V5)
confusionMatrix
