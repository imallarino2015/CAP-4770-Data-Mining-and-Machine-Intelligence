library(FNN)
data(iris)

#Part 1
x=c(5,3,2,1,3)
y=c(2,4,5,2,3)
class=c(T,T,T,F,F)
data=data.frame(x,y,class)

print(data)

png("Q2.png")
plot(data[,2],data[,1],col=ifelse(data[,3],"green","red"))
points(x=2,y=2.5)
dev.off()

#Part 2
#prepare the training and testing data
knnData=iris[,-(1:2)]
knnData$Species=ifelse(knnData$Species=="setosa",0,ifelse(knnData$Species=="versicolor",1,2))
knnTrain=knnData[,-3]
trainClasses=factor(knnData[,3])
knnTest=data.frame(Petal.Length=c(1.8,5.3,7.5),Petal.Width=c(6.8,4.8,6))
testClasses=c("GREEN","GREEN","GREEN")

#preform the predicitions
testClasses=knn(knnTrain,knnTest,trainClasses,k=3,prob=T)
print(knnData)

#plot the data
png("data.png")
plot(knnData$Petal.Width,knnData$Petal.Length,col=c("RED","BLUE","BLACK")[knnData$Species])
dev.off()

#plot the results
png("knn.png")
plot(knnData$Petal.Width,knnData$Petal.Length,col=c("RED","BLUE","BLACK")[knnData$Species])
points(knnTest[,2],knnTest[,1],col=c("RED","BLUE","BLACK")[testClasses])
dev.off()

#confusion matrix
tst=knn(knnTrain,knnTrain,trainClasses,k=3,prob=T)
confMatrix=table(tst,trainClasses)
print(confMatrix)
