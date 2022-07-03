# Clustering with kmeans and pam

# Load the library cluster
library(cluster)
data("USArrests") # Load USArrests dataset
# Center and scale the data
X1 <- scale(USArrests, center = TRUE, scale = TRUE)
# Perform kmeans for values of k between 2 and 10
ess <- matrix(nrow=9) # store the values of the total sum of squares within clusters
for(k in 1:9) ess[k]<-kmeans(X1, centers = k+1)$tot.withinss
par(mar=c(4, 4, 1, 1))
# Plot ESS (error sum of squares)
plot(2:10, ess, xlab="k", ylab="ESS", pch=16)

# Plot data using text labels and color with the clusters found
par(mar=c(4, 4, 1, 1))
plot(X1[,3], X1[,2], type="n", xlab="UrbanPop", ylab="Assault")
clus <- kmeans(X1, centers=4)$cluster
text(X1[,3], X1[,2], labels=row.names(X1), cex=0.5, col=clus) # just the variables required

# Do PCA
PCA <- prcomp(USArrests, center=TRUE, scale=TRUE)
summary(PCA)

X2 <- PCA$x[,1:2] # Get just the first to PCs
# Test different values of k and check the ESS graph
ess <- matrix(nrow=9)
for(k in 1:9) ess[k] <-kmeans(X2, center=k+1)$tot.withinss
plot(2:10,ess,xlab="k",ylab="ESS",pch=16)

# Construct scatter plots with text labels
par(mar=c(4,4,1,1),mfrow=c(1,2)) # set the frame
plot(X1[,3], X1[,2], type="n", xlab="UrbanPop", ylab="Assault")
clus <- kmeans(X2, centers=4)$cluster
text(X1[,3], X1[,2], labels=row.names(X1), cex=0.5, col = clus)
plot(X2[,1], X2[,2], type="n", xlab="PC1", ylab="PC2")
text(X2[,1], X2[,2], labels=row.names(X2), cex=0.5, col=clus)


# Use the iris dataset with centered but unscaled data and without the 5th column
X3 <- scale(iris[, -5], center=TRUE, scale=FALSE)
par(mar=c(4,4,1,1),mfrow=c(2,2))
for(i in 1:4) plot(X3[,1], X3[,2], pch=16, col=pam(X3, k=1+i)$clustering, main=i+1,
                   xlab=colnames(X3)[1], ylabl=colnames(X3)[2])

# Repeat the step with scaled data
X3 <- scale(iris[, -5], center=TRUE, scale=TRUE)
par(mar=c(4,4,1,1),mfrow=c(2,2))
for(i in 1:4) plot(X3[,1], X3[,2], pch=16, col=pam(X3, k=1+i)$clustering, main=i+1,
                   xlab=colnames(X3)[1], ylabl=colnames(X3)[2])

# Let's do it with the first two PCs
PCA <- prcomp(iris[, -5], center=TRUE, scale=TRUE)
X4 <- PCA$x[,1:2]
par(mar=c(4,4,1,1),mfrow=c(2,2))
for(i in 1:4) plot(X4[,1], X4[,2], pch=16, col=pam(X4, k=i+1)$clustering,
                   main=i+1, xlab="PC1", ylab="PC2")
