library(cluster)
X <- c(6.2, 5, 3.9, 5.8, 4.8, 4.1, 4.1, 4.9, 1.2, 5.7, 2.3, 4.8, 5.8, 1.1, 4.9, 6.3, 3.9, 1.3, 5, 3.8, 5, 3.7, 5, 6.2, 1.3, 6.1, 0.7)
dim(X) <- c(9, 3)
X
dist(X, method="manhattan", diag=TRUE, upper=TRUE)

D <- as.matrix(dist(X, method="manhattan", diag=TRUE, upper=TRUE))
min(D[c(1,7,9), -c(1,7,9)])

A <- agnes(X, method="single", metric="manhattan")
A$diss
plot(A, which.plots = 2, main="Single", cex.main=0.75, hang=-1)

KM <- kmeans(X, centers=3)
KM$cluster
KM$centers


PM <- pam(X, k=3)
PM$id.med
PM$medoids

X_1 <- diag(c(45.497, 10.864, 10.459, 10.014, 9.046, 8.607, 8.089, 7.948, 7.77, 7.348, 7.078, 6.69))
X_1 <- (1/114)*X_1^2
diag(X_1)[7]/sum(diag(X_1)) * 100
diag(X_1)/sum(diag(X_1)) * 100

T <- c(4.8, 6.2, 4.7, 0.9, 1.7, 6.3, 4.1, 5.1, 4.3, 1.2, 2, 2.8, 5.7, 6, 2.8, 4.1, 0.9, 0.8, 2, 0.8, 5.1, 5.1, 1.3, 5.7, 2.9, 3.9, 3.7, 4.8, 6, 1.1, 0.8, 1.1)
dim(T) <- c(8, 4)
D_2 <- as.matrix(dist(T, method="manhattan", diag = TRUE, upper = TRUE))
max(D_2[c(5,6),-c(5,6)])
