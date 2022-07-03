# Agglomerative clustering using the library cluster

library(cluster)
# Create the data matrix
X <- matrix(c(0, 3, 6, 0, 1, 4, 6, 2, 5, 1), nrow=5, ncol=2)
X

# Build a distance matrix
dist(X, method="manhattan", diag=TRUE, upper=TRUE) # Manhattan
dist(X, method="euclidian", diag=TRUE, upper=TRUE) # Euclidean
dist(X, method="minkowski", diag=TRUE, upper=TRUE, p=4) # Minkowski

# Do agglomerative clustering
A <- agnes(X, method="complete", metric="euclidian")
# Construct a dendogram
par(mar=c(4,4,1,1))
plot(A, which.plot=2, cex.main=0.4)

# Verify the distance between clusters 14 and 2 match the dendrogram
D <- as.matrix(dist(X, method="euclidean", diag=TRUE, upper=TRUE))
D[c(1,4), 2] # Just checking the relevant distances
max(D[c(1,4), 2]) # Get the greatest distance
# Check it matches the dendrogram
A$height # Correct

# Consider the dataset ruspini
head(ruspini)
par(mar=c(4,4,1,1))
plot(ruspini, type='n')
text(ruspini$x, ruspini$y, labels=row.names(ruspini), cex=0.5)

# Perform agglomerative clustering using euclidean and avg. linkage
A <- agnes(ruspini, method="average", metric="euclidean")
plot(A, which.plot=2, cex=0.5)

# Change parameters of the clustering
A <- agnes(ruspini, method="complete", metric="euclidean")
plot(A, which.plot=2, cex=0.5)
