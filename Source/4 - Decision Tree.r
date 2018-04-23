library(rpart)
library(ROCR)
data(iris)

#Part 1
#setup data
set.seed(98)
training=sample(1:nrow(iris),.8*nrow(iris))

#decision tree creation
dTree=rpart(Species~Petal.Width,data=iris[training,],method="class")
prediction=predict(dTree,iris[-training,],type="class")

#calculate tree accuracy
confMat=table(iris[-training,]$Species,prediction)
tpr=confMat[1,1]/(confMat[1,1]+confMat[1,2])
fpr=confMat[2,1]/(confMat[2,1]+confMat[2,2])
acc=(confMat[1,1]+confMat[2,2])/
(confMat[1,1]+confMat[1,2]+confMat[2,1]+confMat[2,2])
print(confMat)
print(tpr)
print(fpr)
print(acc)

#decision tree
png("decisionTree.png")
plot(dTree,uniform=T)
text(dTree,use.n=T,all=T,cex=.8)
dev.off()
print(summary(dTree))

#determine probabilities
probabilities=predict(dTree,iris[-training,],type="prob")
probabilityLabels=cbind(probabilities[,2],iris[-training,]$Species)

#calculate performance
probPrediction=prediction(probabilityLabels[,1],probabilityLabels[,2])
dTreePerformance=performance(probPrediction,"tpr","fpr")

#plot roc
png("dTreePerformance.png")
plot(dTreePerformance,col="black")
abline(0,1,col="lightgray")
dev.off()

#calculate area under the curve
auc=performance(probPrediction,"auc")
print(auc@y.values[[1]])

#label prediction
sample=data.frame(Rm=c(6.2),Lstat=c(3.5))
sampleClass=predict(dTree,sample,type="class")
print(sampleClass)

#Part 2
#decision tree creation
dTreeAll=rpart(Species~.,data=iris[training,],method="class")
predictionAll=predict(dTreeAll,iris[-training,],type="class")

#determine probabilities
probabilitiesAll=predict(dTreeAll,iris[-training,],type="prob")
probabilityLabelsAll=cbind(probabilitiesAll[,2],iris[-training,]$Species)

#calculate performance
probPredictionAll=prediction(probabilityLabelsAll[,1],probabilityLabelsAll[,2])
dTreePerformanceAll=performance(probPredictionAll,"tpr","fpr")

#plot roc
png("dTreePerformanceComp.png")
plot(dTreePerformance,col="black")
plot(dTreePerformanceAll,add=T,col="red")
abline(0,1,col="lightgray")
dev.off()
