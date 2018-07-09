library(xlsx)
library(psych)
library(NbClust)

# Set the working directory
setwd("C:/Users/cher0_000/Desktop/CSC424/Final Project")

# Read in the house csv "Parkinsons" dataset.
file = read.xlsx("Data - Parkinsons.xlsx", 1, to.data.frame=T, header = TRUE)
head(file, 10)
data = file[1:195,]

# Choose appropriate variables
parkinsons1 = data[, c(2:24)] # keep status for comparison
parkinsons = data[, c(2:17, 19:24)] #remove status for analysis

str(parkinsons)

# Check frequencies and descriptive statistics for all variables
describe(parkinsons)

# Prepare Data
parkinsons <- na.omit(parkinsons) # listwise deletion of missing
parkinsons
plot(parkinsons, main = "Scatterplot of 22 variables on Parkinson's dataset")
is.data.frame(parkinsons)
m <- as.matrix(parkinsons)
m
m <- mapply(m, FUN=as.numeric)
m <- matrix(data = m, ncol = 22, nrow = 195)
# min-max scaling
m1 <- apply(m, 2, function(x){(x-min(x))/(max(x)-min(x))})
m1

###############################################################
#Hierarchical Agglomerative Clustering
###############################################################

# Calculate Euclidean distance
d <- dist(m1)
as.matrix(d)[1:195,1:22]

# Single-linkage clustering
fit.single <- hclust(d, method = "single")
plot(fit.single, hang = -1, cex = .8, main = "Single Linkage Clustering")

# Complete-linkage clustering
fit.complete <- hclust(d, method = "complete")
plot(fit.complete, hang = -1, cex = .8, main = "Complete Linkage Clustering")

# Average-linkage clustering
fit.average <- hclust(d, method = "average")
plot(fit.average, hang = -1, cex = .8, main = "Average Linkage Clustering")

# Ward's clustering
fit.ward <- hclust(d, method = "ward.D")
plot(fit.ward, hang = -1, cex = .8, main = "Ward's Method Clustering")

# Centroid clustering
fit.centroid <- hclust(d, method = "centroid")
plot(fit.centroid, hang = -1, cex = .8, main = "Centroid Method Clustering")

# Obtain final cluster solution
clusters <- cutree(fit.ward, k = 3)
table(clusters)

aggregate(m, by = list(cluster = clusters), mean)
aggregate(m1, by = list(cluster = clusters), mean)

plot(fit.ward, hang = -1, cex = .8,
     main = "Ward's Method Clustering\n3 Cluster Solution")
rect.hclust(fit.ward, k = 3)

# Visualize scree plot
library(factoextra)
fviz_nbclust(m1, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

###############################################################
# K-means Partitioning Clustering
###############################################################
set.seed(12345)
devAskNewPage(ask=TRUE)
nc <- NbClust(m1, min.nc = 2, max.nc = 15, method =  "kmeans")
table(nc$Best.n[1,])
fviz_nbclust(nc) + theme_minimal()

# Elbow method for determining the optimal number of clusters
set.seed(1234)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- m1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

# Visualize scree plot
fviz_nbclust(m1, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# k-means clustering
set.seed(1234)
fit.km <- kmeans(m1, 3, nstart = 25)
fit.km$size
fit.km$centers
aggregate(m, by = list(cluster = fit.km$cluster), mean)

# Visualize k-means clusters
fviz_cluster(fit.km, data = m1, geom = "point",
             stand = FALSE, frame.type = "norm")

# K-means performance on uncover the actual structure
ct.km <- table(parkinsons1$status, fit.km$cluster)
ct.km

###############################################################
# PAM Partitioning Clustering
###############################################################
library(cluster)
fit.pam <- pam(m1, 3)
fit.pam$cluster

# Visualize pam clusters
fviz_cluster(fit.pam, stand = FALSE, geom = "point",
             frame.type = "norm")

# K-means performance on uncover the actual structure
ct.km <- table(parkinsons1$status, fit.pam$cluster)
ct.km

###############################################################
#Validate Clustering
###############################################################
library(clValid)
# Internal Validation
internal <- clValid(m1,2:4, clMethods = c("hierarchical","kmeans", "pam"),
                    validation = "internal")
summary(internal)

op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(internal, legend=FALSE)
plot(nClusters(internal),measures(internal,"Dunn")[,,1],type="n",axes=F,xlab="",ylab="")
legend("bottom", clusterMethods(internal), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

# Stability Validation
stab <- clValid(m1, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                validation = "stability")
summary(stab)

par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(stab, measure=c("APN","AD","ADM"),legend=FALSE)
plot(nClusters(stab),measures(stab,"APN")[,,1],type="n",axes=F,xlab="",ylab="")
legend("bottom", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
par(op)