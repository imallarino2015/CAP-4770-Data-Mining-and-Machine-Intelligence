library(MASS)
data(Boston)
attach(Boston)

#Part 1
print(dim(Boston))

#Part 2
print(mean(rm))
print(median(rm))
print(sd(rm))
print(var(rm))
print(mean(medv))
print(median(medv))
print(sd(medv))
print(var(medv))
print(cov(rm,medv))

#Part 3
png("medvHistogram.png")
hist(medv)
dev.off()
png("medvDensity.png")
plot(density(medv))
dev.off()

#Part 4
sink("bostonCorrelation.txt")
print(cor(Boston))
sink()

#Part 5
png("medv.png")
par(mfrow=c(2,2))
plot(medv)
plot(medv[rm>4])
plot(medv[rm>4&crim<1])
plot(medv[rm>4&crim<1&age>=50&age<=80])
dev.off()

#Part 6
png("medvCorrelation.png")
par(mfrow=c(4,4))
plot(medv,crim)
plot(medv,zn)
plot(medv,indus)
plot(medv,chas)
plot(medv,nox)
plot(medv,rm)
plot(medv,age)
plot(medv,dis)
plot(medv,rad)
plot(medv,tax)
plot(medv,ptratio)
plot(medv,black)
plot(medv,lstat)
dev.off()

#Part 7
Boston.Subset=Boston[seq(1,dim(Boston)[1],length=dim(Boston)[1]/2)+1,c(2,6,13,14)]
print(head(Boston.Subset))

detach(Boston)
