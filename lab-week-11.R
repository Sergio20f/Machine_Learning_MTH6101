# Diabetes analysis using lasso & lasso cross-validation
install.packages("lars")

library(lars)
library(cvTools)

# Loading the dataset
data("diabetes")
attach(diabetes)
X <- as.matrix(diabetes)
X <- scale(X[,1:11], center=TRUE, scale=TRUE) # scale the data
CX <- colnames(X) # store the column names
# Remove the "x." from the column names
for(i in 1:10) CX[i] <- substr(CX[i], 3, nchar(CX[i]))
colnames(X) <- CX
# Create the dataframe
DAT <- data.frame(X)
# Split the data in four partition folds
set.seed(0)
n <- nrow(X)
K <- 4
CV <- cvFolds(n, K)
train <- CV$subsets[CV$which!=K]
test <- CV$subsets[CV$which==K]

# Fit lasso analysis
LS <- lars(x=X[train, -c(11)], y=X[train, 11], type="lasso", intercept=FALSE, 
           normalize=FALSE)

# Build lasso predictions and store them
PL <- predict(LS, X[test, -c(11)], type="fit")
# Matrix of real labels
Yobs <- matrix(nrow=nrow(DAT[test,]), ncol=ncol(PL$fit), byrow=FALSE, DAT$y[test])
# Compute the MSE
MSEL <- apply((Yobs-PL$fit)^2, 2, mean)

# Plot the MSE at breakpoints
par(mar=c(4,4,1,1))
plot(MSEL, xlab="Breakpoint", ylab="MSE")
lines(which.min(MSEL)*c(1,1),range(MSEL),col="gray",lty=2) # Spot the minimum

which.min(MSEL)
min(MSEL)

# Find the coefficients at minimal MSE
cbind(LS$beta[which.min(MSEL),], lm(y~.-1, data=DAT[train,])$coefficients)
# Compute the percentage of shrinkage at the minimum MSE
SO <- sum(abs(LS$beta[which.min(MSEL),]))/sum(abs(lm(y~.-1, data=DAT[train,])$coefficients))
SO*100

# Plot the lasso path
plot(LS, lwd=2) ## together with selected coefficients
lines(x=c(1, 1)*SO, range(LS$beta), lty=2, col="gray", lwd=3)
