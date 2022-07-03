# Exercise 1

X <- c(-1, 1, -1, 0, 1, 0, 1, 0, -1, 0, 1, -1)
dim(X) <- c(3, 4)
X

svd(X) # Singular value decomposition
# D is a rectangular matrix. In its diagonal contains the sqrt of the eig(XX^T)
D <- diag(svd(X)$d)
# U is a matrix whose columns are the eigenvectors of XX^T
U <- svd(X)$u
# V is a matrix whose columns are the eigenvectors of X^TX
V <- svd(X)$v
# ** \Sigma = XX^T

# Recover original Matrix
X_f <- U %*% D %*% t(V) # t(V) == V^T
round(X_f)

# First component using just the first eigenvalue
D_1 <- c(D[1], 0, 0, 0 ,0, 0, 0, 0, 0)
dim(D_1) <- c(3, 3)

X_1 <- U %*% D_1 %*% t(V)
X_1

# Second component using just the first two terms
D_2 <- c(D[1], 0, 0, 0, D[5], 0, 0, 0, 0)
dim(D_2) <- c(3, 3)

X_2 <- U %*% D_2 %*% t(V)
X_2

# Third component using every term
X_3 <- U %*% D %*% t(V) # t(V) == V^T
round(X_3)

# Comments: The matrix D_1 is a only an approximation, quite a rough one. D_2
# is a more accurate one, it is a low rank approximation to the original matrix.
# D_3 is simply the original matrix as we are taking all the eigenvalues.

# Exercise 2
A <- c(-1, 1)
dim(A) <- c(1, 2)

B <- t(A) %*% A

# Exercise 3
data("iris")
summary(iris)

X <- iris[, -c(4, 5)] # Grabbing just relevant classes
dta <- scale(X, center=TRUE, scale=FALSE) # Centering but not scalling
PCA <- prcomp(X, center=TRUE, scale=FALSE) # Performing PCA
summary(PCA)

eigenvalues <- svd(dta)$d
eigenvalues
dim(X)
# The relation between the eigenvalues found and the standard deviation comp. of
# the PCA analysis is given by: sqrt(\lambda_i) = (1/n-1)*d_i^2
# Where \lambda_i are the eigenvalues of the variance-covariance matrix and d_i
# are the eigenvalues of the svd decomposition.

# Exercise 4
library(MASS)
library(lars)
library(cvTools)
data("gilgais")

set.seed(0)
summary(gilgais)
X_4 <- gilgais[, -c(2, 3)]
X_4 <- scale(X_4, center=TRUE, scale=TRUE)

# Perform lasso fit
LS <- lars(x=X_4[, -c(1)], y=X_4[, 1], type="lasso", intercept=FALSE,
     normalize=FALSE)

round(LS$beta, 4)
round(LS$lambda, 4)

plot(LS)