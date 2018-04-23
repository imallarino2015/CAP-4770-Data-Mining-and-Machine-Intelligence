library(MASS)
data(Boston)
attach(Boston)

#Part 1
png("Q2.png")
plot(c(300,380,160,580,400),c(25,27,10,42,38))
abline(a=1653/4696,b=7232/93920)
dev.off()

#Part 2
l=lm(medv~rm,data=housing)

png("lmMedv~Rm.png")
par(mfrow=c(2,2))
plot(l)
dev.off()

png("housingLM.png")
plot(rm,medv)
abline(l)
dev.off()

print(l)
print(summary(l))

#Part 3
low=lm(medv~1,data=housing)
high=lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data=housing)

lmstep=step(low,scope=list(lower=low,upper=high),direction="forward",steps=2)
print(summary(lmstep))

detach(Boston)
