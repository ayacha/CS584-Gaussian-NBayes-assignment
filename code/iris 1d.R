setwd("/Users/ayacha/Desktop/cs584-assignment2")

data<-read.table("iris.dataCopy.txt", sep=',')

dim(data)

test_idx <- seq(41, 60)
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]


x = trainingSet[1]
xtest = testingSet[1]
y = trainingSet[5]

sss = unique(y)

m1=sum(y=='Iris-setosa')
                #####x
muu1 = aggregate(trainingSet[1], by=list(trainingSet$V5=='Iris-setosa'), sum)
mu1 = (1/m1)* muu1[2,2]
siig1 = aggregate((trainingSet[1]-mu1)^2, by=list(trainingSet$V5=='Iris-setosa'), sum)
sig1 =(1/m1)* siig1[2,2]

m2=sum(y=='Iris-versicolor')
muu2 = aggregate(trainingSet[1], by=list(trainingSet$V5=='Iris-versicolor'), sum)
mu2 = (1/m2)* muu2[2,2]
siig2 = aggregate((trainingSet[1]-mu1)^2, by=list(trainingSet$V5=='Iris-versicolor'), sum)
sig2 =(1/m2)* siig2[2,2]


#Fitting model

testYpredictions1 = (1/sig1)* exp(-0.5* ((xtest-mu1)^2) / (sig1^2))

#Training error
trainYpredictions1 = (1/sig1)* exp(-0.5* ((x-mu1)^2) / (sig1^2))


testYpredictions2 = (1/sig2)* exp(-0.5* ((xtest-mu2)^2) / (sig2^2))

#Training error
trainYpredictions2 = (1/sig2)* exp(-0.5* ((x-mu2)^2) / (sig2^2))


yhat1 <- ifelse(testYpredictions1 > testYpredictions2, "Iris-setosa", "Iris-versicolor")

confusionMatrix=table(yhat1, testingSet$V5)
confusionMatrix
