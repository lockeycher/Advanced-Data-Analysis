# Student name: Lavinia Wang
# ASsignment 1
  
# Problem 1:
Z = matrix(c(1, 4, 1, 6, 1, -3, 1, 2), nrow = 4, ncol = 2, byrow = T)
Z
Y = matrix(c(-1, 1, 7, 8), nrow = 4, ncol = 1, byrow = F)
Y
M = matrix(c(1, 5, 0, 30, 50, 15, 0, 20, 10), nrow = 3, ncol = 3, byrow = T)
M
N = matrix(c(-20, -5, 0, 0, 20, 5, 20, 10, 20), nrow = 3, ncol = 3, byrow = T)
N
v = matrix(c(-2, 2, 6), nrow = 3, ncol = 1, byrow = F)
v
w = matrix(c(3, -7, 5), nrow = 3, ncol = 1, byrow = F)
w

# a. v???w(dot product)
dim(v)
dim(w)
prod = t(v) %*% w
prod

# b. ???3*w
prod1 = -3* w
prod1

# c. M*v
dim(M)
dim(v)

# d. M+N
sum = M + N
sum

# e. M-N
subtr = M - N
subtr

# f.Z^T * Z
ZTZ = t(Z) %*% Z
ZTZ

# g. (Z^T * Z) ^-1
ZTZI = solve(ZTZ)
ZTZI

#h.Z^T * Y
ZTY = t(Z) %*% Y
ZTY

#i.(Z ^T * Z) ^ -1 * Z^T * Y
beta = ZTZI %*% ZTY
beta

#j.det(Z ^T * Z)
det= det(ZTZ)
det


#Problem 2
library(car)
library(Hmisc)
library(corrplot)
library(QuantPsyc)
library(leaps)
# Read in the house csv "housedata" dataset.
house = read.csv(file = "C:/Users/cher0_000/Desktop/CSC424/HW 1/housedata.csv", header = TRUE)
str(house)
head(house)

#Check frequencies and missing values for all variables or a specific variable
describe(house)

# Pull out just the numeric fields and place price at the front 
# Change Variable Names

houseNum = house[, c(3, 4:21)]
head(houseNum)
plot(houseNum)

#Check data types
str(houseNum)

# Compute the correlation matrix and visualize it
cor.houseNum = cor(houseNum)
cor.houseNum
corrplot(cor.houseNum, method="ellipse")

# Try a fit of the full set of parameters
# There are aliased coefficients in the model. 
# Remove the variable that causes error
fullFit = lm(price ~ .-sqft_basement, data = houseNum)
summary(fullFit)

lm.beta.fullfit <- lm.beta(fullFit)
lm.beta.fullfit

# Compute the vif scores to get an idea of the multicolinearities
vif(fullFit)

###################################################################
# Automated fitting
###################################################################

houseSubsets = regsubsets(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors 
                          + waterfront + view + condition + grade + sqft_above + yr_built 
                          + zipcode + lat + long + sqft_living15 + sqft_lot15, 
                          data = houseNum, nbest=10)
houseSubsets

plot(houseSubsets, scale="adjr2")

bestR2Fit = lm(price ~ sqft_living + waterfront + view + grade + yr_built 
               + lat, data = houseNum)
summary(bestR2Fit)

# The R function "step" can perform stepwise regression, but to get going, we need to feed 
# it the two "bounding" models so that it knows what to work with
null = lm(price ~ 1, data = houseNum)
null

full = lm(price ~ ., data = houseNum)
full

# First we do a forward search - Forward Stepwise
houseForward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(houseForward)
# Compare the results to the full search above

# Next do a backward search - Backward Stepwise
houseBackward = step(full, direction="backward")
summary(houseBackward)

# Finally we do a "stepwise" search combining the two - Both Forward and Backward Stepwise
houseStep = step(null, scope = list(upper=full), direction="both")
summary(houseStep)
summary(houseForward)
summary(houseBackward)
vif(houseStep)
vif(houseForward)
vif(houseBackward)


