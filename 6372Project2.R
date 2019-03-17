require("mlbench")
require("corrplot")
data(PimaIndiansDiabetes2,package = "mlbench") #see note about correction to dataset at https://www.rdocumentation.org/packages/mlbench/versions/2.1-1/topics/PimaIndiansDiabetes
Pima <- PimaIndiansDiabetes2;
rm(PimaIndiansDiabetes2)

head(Pima)
summary(Pima)
levels(Pima$diabetes)<-c(0,1) #replacing diabetes condition of 'neg' or 'pos' with 0 and 1, respectively. 
Pima$diabetes<-as.numeric(Pima$diabetes) #changing diabetes from a factor to numeric
Pima<-Pima[complete.cases(Pima),] #using only complete records

PimaCorr<-cor(Pima)
corrplot(PimaCorr, method="number")

par(mfrow=c(3,3))  #creating histograms of the variables.
for (i in 1:9){hist(Pima[,i], col=rgb(0,0,.9, .7), xlab="", main = (colnames(Pima[i])))}
par(mfrow=c(1,1))

a<-princomp(Pima[,1:8]) #PCA of dataset, leaving out diabetes variable
a
a$loadings #12.5% cumulative variance explained by Comp.1, 87.5% by Comp.7.
biplot(a) #this shows a large eigenvector for insulin, but is messy to read.
X <- a$loadings #this provides a much cleaner view with datapoints removed
plot(X, asp=1, type="n")
abline(v=0, lty=3)
abline(h=0, lty=3)
arrows(0, 0, X[,1], X[,2], len=0.1, col="red")
text(1.1*X, rownames(X), col="red", xpd=T)
print(a)

