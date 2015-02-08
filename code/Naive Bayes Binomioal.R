setwd("/Users/ayacha/Desktop/cs584-assignment2")

data<-read.table("CNAE-9.data.txt", sep=',')

dim(data)

test_idx <- seq(1, 100)
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]


x = trainingSet[-1]
xtest = testingSet[-1]
y = trainingSet[1]
ytest = testingSet[1]


m= dim(x)[1] ####
n= dim(x)[2]
alphaaa1= aggregate(x, by=list(y==1), sum)

alphaaa1 = alphaaa1[2,]
alpha1 = (1/m)*alphaaa1[-1]

Pi = rowSums(x)
PiTest = rowSums(xtest)

trainYpredictions1 = c(rep(1, m))
seq = dim(x)[1]
for(i in 1:m) {
  for(j in 1:n) {
    if(y[i,]==1){
      trainYpredictions1[i] = trainYpredictions1[i] * (choose(ceiling(x[i,j]), ceiling(Pi[i])) * (alpha1[j])^(x[i,j]) * (1-alpha1[j])^(Pi[i]-x[i,j]));
    }
  }
}

m= dim(xtest)[1] ####

testYpredictions1 = c(rep(1, m))
seq = dim(xtest)[1]
for(i in 1:m) {
  for(j in 1:n) {
    if(y[i,]==1){
      testYpredictions1[i] = testYpredictions1[i] * (choose(ceiling(xtest[i,j]), ceiling(PiTest[i])) * (alpha1[j])^(xtest[i,j]) * (1-alpha1[j])^(PiTest[i]-xtest[i,j]));
    }
  }
}








m= dim(x)[1] ####
n= dim(x)[2]
alphaaa2= aggregate(x, by=list(y==2), sum)

alphaaa2 = alphaaa2[2,]
alpha2 = (1/m)*alphaaa2[-1]

Pi = rowSums(x)
PiTest = rowSums(xtest)

trainYpredictions2 = c(rep(1, m))
seq = dim(x)[1]
for(i in 1:m) {
  for(j in 1:n) {
    if(y[i,]==1){
      trainYpredictions2[i] = trainYpredictions2[i] * (choose(ceiling(x[i,j]), ceiling(Pi[i])) * (alpha2[j])^(x[i,j]) * (1-alpha2[j])^(Pi[i]-x[i,j]));
    }
  }
}

m= dim(xtest)[1] ####

testYpredictions2 = c(rep(1, m))
seq = dim(xtest)[1]
for(i in 1:m) {
  for(j in 1:n) {
    if(y[i,]==2){
      testYpredictions2[i] = testYpredictions2[i] * (choose(ceiling(xtest[i,j]), ceiling(PiTest[i])) * (alpha2[j])^(xtest[i,j]) * (1-alpha2[j])^(PiTest[i]-xtest[i,j]));
    }
  }
}






yhat <- ifelse(testYpredictions1 > testYpredictions2, 1, 2)
confusionMatrix=table(as.matrix(yhat), as.matrix(ytest))
confusionMatrix
