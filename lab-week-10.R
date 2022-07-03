# Analyse the diabetes data set using ridge regression
install.packages("ridge")

library(ridge)
library(lars)
library(cvTools)

# Load the data
data("diabetes")
X <- as.matrix(diabetes)
X <- scale(X[, 1:11], center=TRUE, scale=TRUE)
CX <- colnames(X)
for(i in 1:10) CX[i]<-substr(CX[i],3,nchar(CX[i])) # remove ".x"
DAT <- data.frame(X)

# Create validation index variables train and test for a 3:1 partition
set.seed(0)
n <- nrow(X); K <- 4
CV <- cvFolds(n, K)
train <- CV$subsets[CV$which!=K]
test <- CV$subsets[CV$which==K]

# Create ridge coefficients
rangelambda <- 10^seq(from=-9, to=6, length.out=200) # linear in log-scale

# Do Ridge regression
LR <- linearRidge(y~.-1, data=DAT[train,], lambda=rangelambda,
                  scaling="scale")

# Cannot proceed - there is no package called ‘ridge’
