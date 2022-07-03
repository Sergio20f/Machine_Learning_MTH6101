# Doing PCA of the olive oil dataset.

# Install pdfCluster
install.packages('pdfCluster')
# Load the library
library(pdfCluster)
# Load the data
data(oliveoil)
# Check the summary of the dataset
summary(oliveoil)

# Check the ranges of each of the variables
apply(oliveoil, 2, range)

# Do the matrix of scatterplots
pairs(oliveoil, pch=16, cex=0.5) # Hard to interpret, many variables

# Remove the first two variables
X <- oliveoil[, -c(1:2)]
pairs(X, pch=16, cex=0.5)
# Include color in the plot
pairs(X, pch=16, cex=0.5, col=oliveoil$macro.area) # color macro.area
pairs(X, pch=16, cex=0.5, col=oliveoil$region) # color region

# Center the data
X <- scale(X, center=TRUE, scale=FALSE)
round(var(X), 5)

round(100*diag(var(X))/sum(diag(var(X))), 5) # We can expect the larger variances to be the first PCs

# Do PCA
PCA <- prcomp(X, center=TRUE, scale=FALSE)
summary(PCA)

# Look at the PC loadings
PCA$rotation
# For interpretation, rounding the loadings can help removing small coefficients
round(PCA$rotation, 1)

# Do a biplot
biplot(PCA, cex=0.5)

#------------------------Repeat with scale=TRUE--------------------------------
X_2 <- oliveoil[, -c(1:2)]
X_2 <- scale(X_2, center=TRUE, scale=TRUE)
round(var(X_2), 5) # Because the data is scaled it has unit variance
round(100*diag(var(X_2))/sum(diag(var(X_2))), 5)
# Because of the unit variance, it is no longer possible to determine a priori the impact of the variables

PCA_2 <- prcomp(X_2, center=TRUE, scale=FALSE)
summary(PCA_2)

# Check the loadings
round(PCA_2$rotation, 1)
biplot(PCA_2, cex=0.5)
