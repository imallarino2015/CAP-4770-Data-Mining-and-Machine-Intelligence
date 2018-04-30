library(rpart)
library(ROCR)
library(ada)
library(MASS)
data(Boston)

#Part 1
#setup data
set.seed(1)
data=Boston
data$medv=ifelse(Boston$medv>21,1,0)
training=sample(1:nrow(data),.8*nrow(data))

#Part 2
#decision tree creation
dTree=rpart(medv~rm+lstat,data=data[training,],method="class")

#calculate tree accuracy
prediction=predict(dTree,data[-training,],type="class")
confMat=table(data[-training,]$medv,prediction)
acc=(confMat[1,1]+confMat[2,2])/
    (confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2])

print(confMat)
print(acc)

#Part 3
#build bagging models
n=c(5,10,20)
baseClassifiers=list()
for(i in 1:length(n)){
    currentClassifiers=list()
    for(j in 1:n[i]){
        samp=sample(training,size=length(training),replace=T)
        bag=data[samp,]
        currentClassifiers=c(currentClassifiers,list(rpart(medv~.,data=bag,method="class")))
    }
    baseClassifiers=append(baseClassifiers,list(currentClassifiers))
}

#test bagging models' accuracies
for(a in 1:length(n)){
    votes=NULL
    for(b in 1:n[a]){
        baseClassifier=baseClassifiers[[a]][[b]]
        pred=predict(baseClassifier,data[-training,])
        label=ifelse(pred>.5,1,0)
        if(is.null(votes))
            votes=label
        else
            votes=votes+label
    }
    decision=ifelse(votes[,1]<votes[,2],1,0)
    confMat=table(data[-training,]$medv,decision)
    acc=(confMat[1,1]+confMat[2,2])/
        (confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2])
    
    print(paste("Bagging ",n[a]," models:",sep=""))
    print(confMat)
    print(acc)
}

#Part 4
adaData=data
for(a in 1:length(n)){
    adaModel=ada(medv~.,adaData[training,],loss="exponential",type="discrete",iter=n[a])
    pred=predict(adaModel,adaData[-training,])
    confMat=table(adaData[-training,]$medv,pred)
    acc=(confMat[1,1]+confMat[2,2])/
        (confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2])

    print(paste("Adaptive boosting ",n[a]," models:",sep=""))
    print(confMat)
    print(acc)
}
