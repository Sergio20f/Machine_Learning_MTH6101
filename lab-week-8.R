# Comparing three different classifiers
install.packages("ISLR")
install.packages("cvTools")
install.packages("GGally")

library("cvTools")
library("ISLR")
library("GGally")

# Set a random seed
set.seed(0)
n <- 10000; k <- 5
CV <- cvFolds(n, k)

# Examine the contents of CV
head(CV$which)
head(CV$subsets)

# Split the folds into train and test sets
train <- CV$subsets[CV$which!=5]
test <- CV$subsets[CV$which==5]

# Examine the dataset "Default"
dim(Default)
summary(Default)
head(Default)
# Pair plot
pairs(Default, cex=0.35, pch=16, col=Default$default)
# Advanced pairs plot
ggpairs(Default, ggplot2::aes(colour=Default$default))

# Model to predict default as a function of balance
M1 <- glm(default~balance, family="binomial", data=Default[train,])
# Model to predict default as a function of balance and student
M2 <- glm(default~balance+student, family="binomial", data=Default[train,])
# Model to predict default as a function of all the variables
M3 <- glm(default~., family="binomial", data=Default[train,])

# Predict using the previously defined models
P1 <- predict.glm(object=M1, newdata=Default[test,], type="response")
P2 <- predict.glm(M2, Default[test,], type="response")
P3 <- predict.glm(M3, Default[test,], type="response")

# Build confusion matrices for each case
Ytrue <- Default[test,]$default=="Yes"
Y1 <- P1 > 0.5
Y2 <- P2 > 0.5
Y3 <- P3 > 0.5
table(Ytrue, Y1) # for model 1
table(Ytrue, Y2) # for model 2
table(Ytrue, Y3) # for model 3

# Compute true positive rate and false positive rate for all models
# M1
sum(Ytrue&Y1)/sum(Ytrue) # TPR
sum(!Ytrue&Y1)/sum(!Ytrue) # FPR
# M2
sum(Ytrue&Y2)/sum(Ytrue) # TPR
sum(!Ytrue&Y2)/sum(!Ytrue) # FPR
# M3
sum(Ytrue&Y3)/sum(Ytrue) # TPR
sum(!Ytrue&Y3)/sum(!Ytrue) # FPR
