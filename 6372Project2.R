require("mlbench")
require("corrplot")
require("class")
data(PimaIndiansDiabetes2,package = "mlbench") #see note about correction to dataset at https://www.rdocumentation.org/packages/mlbench/versions/2.1-1/topics/PimaIndiansDiabetes
Pima <- PimaIndiansDiabetes2;
rm(PimaIndiansDiabetes2)
str(Pima)
Pima$diabetes<-as.numeric(Pima$diabetes) #changing diabetes from a factor to numeric
head(Pima)
Pima$diabetes<-Pima$diabetes-1
summary(Pima)
Pima<-Pima[complete.cases(Pima),] #using only complete records

PimaCorr<-cor(Pima)
corrplot(PimaCorr, method="number")

par(mfrow=c(3,3))  #creating histograms of the variables.
for (i in 1:9){hist(Pima[,i], col=rgb(0,0,.9, .7), xlab="", main = (colnames(Pima[i])))}
par(mfrow=c(1,1))

a<-princomp(Pima[,1:9]) 
a
screeplot(a)
a$loadings #11.1% cumulative variance explained by Comp.1, 77.8% by Comp.7.
biplot(a) #this shows a large eigenvector for insulin, but is messy to read.
X <- a$loadings #this provides a much cleaner view with datapoints removed
plot(X, asp=1, type="n")
abline(v=0, lty=3)
abline(h=0, lty=3)
arrows(0, 0, X[,1], X[,2], len=0.1, col="red")
text(1.1*X, rownames(X), col="red", xpd=T)
print(a)


#repeating above, but with scaled data
Pima[,1:9]<-scale(Pima[,1:9])
a<-princomp(Pima[,1:9]) 
a
screeplot(a)
a$loadings #11.1% cumulative variance explained by Comp.1, 77.8% by Comp.7.
biplot(a) 
X <- a$loadings #this provides a much cleaner view with datapoints removed. Glucose and Pressure are directly aligned with diabetes.
plot(X, asp=1, type="n")
abline(v=0, lty=3)
abline(h=0, lty=3)
arrows(0, 0, X[,1], X[,2], len=0.1, col="red")
text(1.1*X, rownames(X), col="red", xpd=T)
print(a)

#k nearest neighbors on the Pima data set
splits <- sample(1:nrow(Pima),as.integer(0.7*nrow(Pima))) # Split Pima into training and testing
train <- Pima[splits,]
test <- Pima[-splits,]

#3-nearest neighbours
train_labels <- train[, 9]
test_pred <- knn(train = train, test = test,cl = train_labels, k=3)
table(test[,'diabetes'],test_pred) #confusion matrix
confutable<-table(test[,'diabetes'],test_pred)
#for k=3 neighbors:
#test_pred
#   0  1
#0 63 20   confutable[1,1] confutable[1,2]
#1 12 23   confutable[2,1] confutable[2,2]

knndf<-data.frame()
for (i in 1:50) {test_pred <- knn(train = train, test = test,cl = train_labels, k=i)
confutable<-table(test[,'diabetes'],test_pred)
  knndf[i,1]<-i
  knndf[i,2]<-(confutable[1,1]+confutable[2,2])/118
  knndf[i,3]<-(confutable[1,2]+confutable[2,1])/118
}

#chart with two y-axis showing accuracy and misclassification. Best value appears to be nn=30. Wow.
par(mar = c(5,5,2,5))
with(knndf, plot(V1, V2, col="red", pch=16, ylab="Accuracy"))
par(new = T)
with(knndf, plot(V1, V3, pch=16, axes=F, xlab=NA, col="blue", ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Misclassification')
legend("topleft", legend=c("Accuracy", "Misclassification"), pch=16, col=c("red", "blue"))
