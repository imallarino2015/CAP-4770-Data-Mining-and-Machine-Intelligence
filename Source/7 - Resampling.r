library(rpart)
library(caret)
data(iris)

#setup data
set.seed(1)
randIrisData=iris[sample(nrow(iris)),]
k=c(10,5)

#Part 4.1
#train k fold decision tree
for(a in 1:length(k)){
    splitData=cut(seq(1,nrow(randIrisData)),breaks=k[a],labels=F)
    acc=c()
    for(b in 1:k[a]){
        testRange=which(splitData==b,arr.ind=T)
        training=iris[-testRange,]
        model=rpart(Species~.,data=training,method="class")
        prediction=predict(model,randIrisData[testRange,])
        labels=ifelse(prediction[,2]>.5,1,0)
        confMat=table(randIrisData[testRange,]$Species,labels)
        acc=c(acc,(confMat[1,1]+confMat[2,2])/
            (confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2]))
    }
    print(paste(k[a]," fold CV accuracy: ",mean(acc),sep=""))
}

#Part 4.2
splitData=cut(seq(1,nrow(randIrisData)),breaks=k[1],labels=F)
#train 10 fold decision tree model and get mean/sd accuracy
acc=c()
for(a in 1:k[1]){
    testRange=which(splitData==a,arr.ind=T)
    training=iris[-testRange,]
    model=rpart(Species~.,data=training,method="class")
    prediction=predict(model,randIrisData[testRange,])
    labels=ifelse(prediction[,2]>.5,1,0)
    confMat=table(randIrisData[testRange,]$Species,labels)
    acc=c(acc,(confMat[1,1]+confMat[2,2])/
        (confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2]))
}
print(paste(k[1]," fold decision tree CV average accuracy: ",mean(acc),sep=""))
print(paste(k[1]," fold decision tree CV accuracy deviation: ",sd(acc),sep=""))

#train 10 fold logistic model and get mean/sd accuracy
acc=c()
for(a in 1:k[1]){
    testRange=which(splitData==a,arr.ind=T)
    training=iris[-testRange,]
    model=glm(Species~.,data=training,family=binomial)
    prediction=predict(model,randIrisData[testRange,],type="response")
    labels=ifelse(prediction>.5,1,0)
    confMat=table(randIrisData[testRange,]$Species,labels)
    acc=c(acc,(confMat[1,1]+confMat[2,2])/
        (confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2]))
}
print(paste(k[1]," fold logistic regression CV average accuracy: ",mean(acc),sep=""))
print(paste(k[1]," fold logistic regression CV accuracy deviation: ",sd(acc),sep=""))

#Part 6
tc=trainControl(method="cv",number=k[1],savePredictions=T)

model1=train(Species~.,data=iris,trControl=tc,method="rpart")
print(model1)

irisNum=iris
irisNum$Species=ifelse(iris$Species=="setosa",0,1)
model2=train(Species~.,data=irisNum,trControl=tc,method="glm",family=binomial(logit))
print(model2)
