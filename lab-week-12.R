# Another look at the lasso methodology
install.packages("glmnet")

library(lars)
library(glmnet)

# Load the dataset provided
X <- as.matrix(read.table("~/Documents/MTH6101/smalldata.txt"))
X

# Lasso analysis
LL <- lars(x=X[,-4], y=X[,4], type="lasso", normalize=FALSE, intercept=FALSE)
par(mar=c(4,4,1,1),mfrow=c(1,1)) # Figure layout
plot(LL)

# Examine the table of coefficients
LL$beta # Table of coefficients
LL$lambda # values of lambda

s <- apply(X=abs(LL$beta), MARGIN=1, FUN=sum) # Manhattan norm
# Combine the elements in a single table
BM <- cbind(c(LL$lambda, 0), LL$beta, s/max(s))
colnames(BM) <- c("lambda", paste("b", 1:3, sep=""), "s")
BM

# Retrieve the coefficients between the breakpoints 5 and 6 of the Lasso fit
coef.lars(object=LL, s=5+c(0:10)/10)

# Add the interpolated coefficients to the lasso plot
par(mar=c(4,4,1,1),mfrow=c(1,1))
plot(LL)
for(i in 0:10){
  CC <- coef.lars(object=LL, s=5+i/10)
  points(sum(abs(CC))/max(s)*c(1, 1, 1), CC, col=i+1, pch=16)
}

# Examine the net penalty
GL <- glmnet(x=X[,-4], y=X[,4]) # Lasso by default
par(mar=c(4,4,1,1),mfrow=c(1,1))
plot(GL)

# Plot the paths for elastic nets for different values of alpha
par(mfrow=c(3,3),mar=c(4,4,1,1))
for(alpha in seq(from=1, to=0, length.out=9))
  plot(glmnet(x=X[,-4], y=X[,4], alpha=alpha), main=alpha)  
