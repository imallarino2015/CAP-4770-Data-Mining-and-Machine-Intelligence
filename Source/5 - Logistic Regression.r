library(rpart)
library(ROCR)
library(MASS)
data(Boston)

#Part 1
#setup data
set.seed(1)
data=Boston
data$medv=ifelse(Boston$medv>21,1,0)
training=sample(1:nrow(data),.8*nrow(data))

#logistic function creation
rmLog=glm(medv~rm,data=data[training,],family=binomial)
lstatLog=glm(medv~lstat,data=data[training,],family=binomial)

#creating test sequences
rmSeq=seq(min(data[-training,]$rm),
	max(data[-training,]$rm),
	len=nrow(data[-training,]))
lstatSeq=seq(min(data[-training,]$lstat),
	max(data[-training,]$lstat),
	len=nrow(data[-training,]))

#testing models
rmPrediction=predict(rmLog,list(rm=rmSeq),type="response")
lstatPrediction=predict(lstatLog,list(lstat=lstatSeq),type="response")

#plot regression lines
png("rmLog.png")
plot(data$rm,data$medv)
lines(rmSeq,rmPrediction,col="red")
dev.off()

png("lstatLog.png")
plot(data$lstat,data$medv)
lines(lstatSeq,lstatPrediction,col="blue")
dev.off()

#determine probabilities
rmProbabilities=predict(rmLog,data[-training,],type="response")
rmResults=ifelse(rmProbabilities>.5,1,0)

lstatProbabilities=predict(lstatLog,data[-training,],type="response")
lstatResults=ifelse(lstatProbabilities>.5,1,0)

#calculate performance
rmProbPrediction=prediction(rmProbabilities,data[-training,]$medv)
rmLogPerformance=performance(rmProbPrediction,"tpr","fpr")

lstatProbPrediction=prediction(lstatProbabilities,data[-training,]$medv)
lstatLogPerformance=performance(lstatProbPrediction,"tpr","fpr")

#plot roc
png("roc.png")
plot(rmLogPerformance,col="red")
plot(lstatLogPerformance,col="blue",add=T)
abline(0,1,col="lightgray")
dev.off()

#calculate area under the curve
rmAuc=performance(rmProbPrediction,"auc")
print(rmAuc@y.values[[1]])

lstatAuc=performance(lstatProbPrediction,"auc")
print(lstatAuc@y.values[[1]])
