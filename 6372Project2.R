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

data(PimaIndiansDiabetes2,package = "mlbench") #see note about correction to dataset at https://www.rdocumentation.org/packages/mlbench/versions/2.1-1/topics/PimaIndiansDiabetes
Pima <- PimaIndiansDiabetes2;
rm(PimaIndiansDiabetes2)
Pima$diabetes<-as.numeric(Pima$diabetes) #changing diabetes from a factor to numeric
Pima$diabetes<-Pima$diabetes-1
Pima<-Pima[complete.cases(Pima),] 

#this plot shows diabetes status (red=no blue=yes) when plotted on age vs. glucose. There are two distinct clusters. kNN does this but with more variables.
plot(x=Pima$glucose, y=Pima$age, pch=16, col=c('Red', 'Blue')[as.numeric(Pima$diabetes+1)], xlab="Glucose", ylab="Age")

#k nearest neighbors on the Pima data set
splits <- sample(1:nrow(Pima),as.integer(0.7*nrow(Pima))) # Split Pima into training and testing
train <- Pima[splits,]
test <- Pima[-splits,]

#20-nearest neighbours
train_labels <- train[, 9]
test_pred <- knn(train = train, test = test,cl = train_labels, k=20)
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
with(knndf, plot(V1, V2, col="red", pch=16, ylab="Accuracy", xlab="Nearest Neighbors"))
par(new = T)
with(knndf, plot(V1, V3, pch=16, axes=F, xlab=NA, col="blue", ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Misclassification')
legend("topleft", legend=c("Accuracy", "Misclassification"), pch=16, col=c("red", "blue"))


summary(aov(Pima$glucose~Pima$diabetes)) #<2e-16
summary(aov(Pima$insulin~Pima$diabetes)) #1.12e-09
summary(aov(Pima$pregnant~Pima$diabetes)) #2.61e-07
summary(aov(Pima$triceps~Pima$diabetes)) #2.79e-07
summary(aov(Pima$mass~Pima$diabetes)) #5.56e-08
summary(aov(Pima$pedigree~Pima$diabetes)) #2.95e-05
summary(aov(Pima$age~Pima$diabetes)) #8.56e-13
summary(aov(Pima$pressure~Pima$diabetes)) #0.000124

barplot(names.arg = c("Glucose","age", "insulin", "mass", "preg","tri" ,"pedigree", "pressure"), height=log(c(2e-16, 8.56e-13, 1.12e-09, 5.56e-08, 2.61e-07, 2.79e-07, 2.95e-05, 0.000124)))


par(mfrow=c(1,3))
boxplot(Pima$glucose~Pima$diabetes, varwidth=T, main="Glucose", xlab="Diabetes")
boxplot(Pima$age~Pima$diabetes, ylim=c(18,60), main="Age", xlab="Diabetes")
boxplot(Pima$insulin~Pima$diabetes, ylim=c(0,300), main="Insulin", xlab="Diabetes")
par(mfrow=c(1,1))
boxplot(Pima$pregnant~Pima$diabetes, main="Pregnancies", xlab="Diabetes")
boxplot(Pima$triceps~Pima$diabetes, main="Triceps", xlab="Diabetes")
boxplot(Pima$mass~Pima$diabetes, main="BMI, 'Mass'", xlab="Diabetes")
boxplot(Pima$pedigree~Pima$diabetes, main="Pedigree", xlab="Diabetes")
boxplot(Pima$pressure~Pima$diabetes, main="Pressure", xlab="Diabetes")


#my own homemade model...kinda logistic regression based on eyeballing the charts. This one is 75.5102% correct
Pima$Guess<-0
Pima$Guess[Pima$glucose>130]<-1
Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
sum(Pima$IsCorrect)/length(Pima$IsCorrect)

#this one is 76.27551% accurate. 
Pima$Guess<-0
Pima$Guess[Pima$glucose>130 & Pima$insulin>80]<-1
Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
sum(Pima$IsCorrect)/length(Pima$IsCorrect)

#this one is 76.27551% accurate. Age doesn't really seem to add more info.
Pima$Guess<-0
Pima$Guess[Pima$glucose>130 & Pima$insulin>80 & Pima$age>20]<-1
Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
sum(Pima$IsCorrect)/length(Pima$IsCorrect)


#making a plot of accuracy of placement based on if we are guessing diabetes just based on glucose levels
Pima$Guess<-0
Pima$Guess[Pima$glucose>59]<-1
Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
sum(Pima$IsCorrect)/length(Pima$IsCorrect)
plot(x=59, y=sum(Pima$IsCorrect)/length(Pima$IsCorrect), xlim=c(59,200), ylim=c(0,1), ylab="Classification Accuracy", xlab="Threshold glucose level")
for (i in 60:200){
  Pima$Guess<-0
  Pima$Guess[Pima$glucose>i]<-1
  Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
  Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
  points(x=i, y=sum(Pima$IsCorrect)/length(Pima$IsCorrect))
}
abline(v=130, col="red")

#making a plot of accuracy of placement based on if we are guessing diabetes just based on age
Pima$Guess<-0
Pima$Guess[Pima$age>18]<-1
Pima$IsCorrect[Pima$age == Pima$Guess]<-1
Pima$IsCorrect[Pima$age != Pima$Guess]<-0
sum(Pima$IsCorrect)/length(Pima$IsCorrect)
plot(x=18, y=sum(Pima$IsCorrect)/length(Pima$IsCorrect), xlim=c(18,80), ylim=c(0,1))
for (i in 19:80){
  Pima$Guess<-0
  Pima$Guess[Pima$age>i]<-1
  Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
  Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
  points(x=i, y=sum(Pima$IsCorrect)/length(Pima$IsCorrect))
}

#making a plot of accuracy now that I think glucose over 130 is diabetes, I want to know about insulin.
Pima$Guess<-0
Pima$Guess[Pima$glucose>130 & Pima$insulin>59]<-1
Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
sum(Pima$IsCorrect)/length(Pima$IsCorrect)
plot(x=59, y=sum(Pima$IsCorrect)/length(Pima$IsCorrect), xlim=c(59,300), ylim=c(0,1))
for (i in 60:300){
  Pima$Guess<-0
  Pima$Guess[Pima$glucose>130 & Pima$insulin>i]<-1
  Pima$IsCorrect[Pima$diabetes == Pima$Guess]<-1
  Pima$IsCorrect[Pima$diabetes != Pima$Guess]<-0
  points(x=i, y=sum(Pima$IsCorrect)/length(Pima$IsCorrect))
}

Pima<-Pima[,1:9]

#here is a logit model with accuracy on average of 77.12%
splits <- sample(1:nrow(Pima),as.integer(0.7*nrow(Pima))) # Split Pima into training and testing
train <- Pima[splits,]
test <- Pima[-splits,]

logitmodel <- glm(train$diabetes ~ . , data=train, family=binomial(link="logit"))
summary(logitmodel)
anova(logitmodel, test="Chisq")

fitted.results <- predict(logitmodel,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$diabetes)
print(paste('Accuracy',1-misClasificError))

#running the logit ten times got me these accuracies. The mean is calculated below. It's 80.51%
mean(0.805084745762712,0.779661016949153,0.771186440677966, 0.796610169491525, 0.745762711864407, 0.771186440677966, 0.796610169491525, 0.745762711864407, 0.796610169491525, 0.847457627118644)
