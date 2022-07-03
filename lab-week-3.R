# Load the air-quality dataset and see the entries with summary()
data(airquality)
summary(airquality) # We see that there are missing values in some of the vars.

# Scatter plot matrix
pairs(airquality, pch=16, cex=0.5) # pch: shape of the data points, cex: spacing between points
var(airquality)

# Remove the missing values
X <- airquality[complete.cases(airquality),]
summary(X)

# Remove variables day and month
X <- X[, -c(5:6)] # the - sign indicates that you remove the specified columns
# Center the data
X <- scale(x=X, center=TRUE, scale=FALSE)

# The data is now ready for PCA
VX <- var(X) # Compute variance-covariance matrix
E <- eigen(VX) # K-L decomposition
E$values/sum(E$values) * 100 # Percentage importance of each eigenvalue in the variability

# Do a pairs plot of the PCs
# PCs: rotated data X%*%E$vectors where E has the results of the K-L decomp.
pairs(X%*%E$vectors, pch=16, cex=0.5) # %*% is the matrix multiplication operator

# They seem to be rotations. Let's check:
E$vectors
# The second component is almost entirely due to the second variable (-0.98)

# Importance of each variable:
diag(VX)

#-----------------------Repeat the process with scale=TRUE-------------------
X <-airquality[complete.cases(airquality),]
X <- X[,-c(5:6)]
X <- scale(x=X, center=TRUE, scale=TRUE)
VX <-var(X)
E <- eigen(VX)
E$values/sum(E$values)*100
E$vectors

pairs(X%*%E$vectors, pch=16, cex=0.5)
