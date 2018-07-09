# Student name: Lavinia Wang
# ASsignment 2

# Problem 4:
library(psych)

#########################################################################
# Set up dataset
#########################################################################
# Read in the csv "bfi" dataset.
bfi = read.csv(file = "C:/Users/cher0_000/Desktop/CSC424/HW 2/bfi.csv", header = TRUE)
str(bfi)

# Check frequencies and missing values for all variables or a specific variable
describe(bfi)

# Pull out just the numeric fields and place price at the front 
# Change Variable Names

bfi_sub = bfi[, c(2:26)]
plot(bfi_sub)

# Check data types
str(bfi_sub)

# list rows of data that have missing values 
bfi_sub[!complete.cases(bfi_sub),]

# create new dataset without missing values 
newbfi <- na.omit(bfi_sub)
newbfi
describe(newbfi)

# Compute the correlation matrix and visualize it
cor.bfi = cor(newbfi)
cor.bfi
corrplot(cor.bfi, method="ellipse")

# Look at the size of the numeric data
dim(newbfi)

# Run a correlation test to see how correlated the variables are.  Which correlations are significant
library(psych)
options("scipen"=100, "digits"=5)
round(cor(newbfi[,]), 2)
MCorrTest = corr.test(newbfi[,], adjust="none")
MCorrTest

M = MCorrTest$p
M

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)

# Compute covariance matrix
cov(newbfi[,])

# Initial PCA
p = prcomp(newbfi, center=T, scale=T)
plot(p)
abline(1, 0)
summary(p)
print(p)
p$rotation
biplot(p)

rawLoadings = p$rotation %*% diag(p$sdev, nrow(p$rotation), nrow(p$rotation))
print(rawLoadings)
v = varimax(rawLoadings)
ls(v)
v

# Use psych package to run PCA
p2 = psych::principal(newbfi, rotate="varimax", nfactors=6, scores=TRUE)
p2
print(p2$loadings, cutoff=.4, sort=T)
p2$loadings
p2$values
p2$communality
p2$rot.mat

v$loadings

# rUN factor analysis to compare results
fit = factanal(newbfi, 6)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)

