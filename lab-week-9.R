# Build different classifiers and compare them
install.packages("pROC")

library(cvTools)
library(class)
library(tree)
library(MASS)
library(pROC)

X <- glass
dim(X)
# Define the labels (Y) and features (X)
Y <- X[,11]; X <- X[,-1]

# Scale and center the data
X <- scale(X, center=TRUE, scale=TRUE)

# Prepare the output of the analysis
Y <- (Y==7)*1
X[,10] <- Y

# Give names to the columns of X
colnames(X) <- c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type")
Y[Y==1] <- "Yes"
Y[Y==0] <- "No"

# Merge X and Y in a data frame
DAT <- data.frame(X, Y)
attach(DAT)
# Now data is ready for analysis

# Set seed
set.seed(0)
# Create train and test sets with k-folds method
CV <- cvFolds(n=nrow(X), K=3)
train <- CV$subsets[CV$which!=3]
test <- CV$subsets[CV$which==3]

# Fit logistic classifier for the response variable "Type"
M1 <- glm(Type~., data=DAT[train, -11], family="binomial")
P1 <- predict.glm(M1, newdata=DAT[test,], type="response")

# Examine the fitted classifier & identify variables that aren't importn
summary(M1) # From the estimated std, we see that Fe and RI aren't imprt
M11 <- glm(Type~.-Fe-RI, data=DAT[train, -11], family="binomial")
P11 <- predict.glm(M11, newdata=DAT[test,], type="response")

# Fit a K nearest neighbors classifier
M2 <- knn(train=X[train,1:9], test=X[test,1:9], cl=Y[train], k=3)
# Only the columns 1:9 contain variables

# Fit a tree classifier
M3 <- tree(as.factor(Y)~., DAT[train, -10])
P3 <- predict(M3, DAT[test,], type="class")

# Fit a linear discriminant classifier
M4 <- lda(Type~., data=DAT[train, -11])
P4 <- predict(M4, DAT[test,])

# Prepare to compare all classifiers
R1 <- roc(response=Type[test], predictor=P1)
R11 <- roc(response=Type[test], predictor=P11)
R4 <- roc(response=Type[test], predictor=c(P4$x))

# Compute the confusion matrix using for the KNN and Tree
T2 <- table(Y[test], M2)
FPR2 <- T2[1,2]/sum(T2[1,]) ; T2[2,2]/sum(T2[2,])->TPR2

T3 <- table(Type[test], P3)
T3[2,2]/sum(T3[2,])->TPR3; T3[1,2]/sum(T3[1,])->FPR3

# Plot ROC curves for logistic and LDA calssifiers and points for tree and KNN
plot(R1, col="black", main="ROC glass data")
plot(R11, col="grey", add=TRUE)
plot(R4, add=TRUE, col="blue")
points(1-FPR2, TPR2, col="red", pch=16)
points(1-FPR3, TPR3, col="green", pch=16)

# Compute the AUC
auc(R1)
auc(R11)
auc(R4)