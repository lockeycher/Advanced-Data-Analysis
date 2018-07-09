# Student name: Lavinia Wang
# ASsignment 2

#Problem 2
setwd("C:/Users/cher0_000/Desktop/CSC424/HW 3")
###################################################################
# Reading the data
###################################################################
# Read in the "canon" dataset
ds = read.table(file = "canon.txt", header=TRUE, sep = "\t", dec = ".")
head(ds)

###################################################################
# Separating the data.  We will segment variables into 
# Attitudinal variables vs Health variables
###################################################################

attitude = ds[c(6:8,10)]
health = ds[c(2:5,9)]

head(attitude)
head(health)

###################################################################
# Now, lets do some computation
###################################################################

# The CCA library has more extensive functionality
library(CCA)

# First invesitgate the combined correlation matrix, and test the 
# cross correlations.  To do this we use the "matcor" function which 
# computes correlation matrices between two datasets
c = matcor(attitude, health)

# Then we pull out the upper right block of correlations that compare
# the attitude and health variables
cCross = c$XYcor[c("esteem","control","attmar","attrole"), 
                 c("timedrs","attdrug","phyheal","menheal","druguse")]
round(cCross, 2)

#Correlations between attitude and attitude (X)
#Correlations between health and health (Y)
cc = cc(attitude, health)
cc$cor

###################################################################
# This is a nice function for computing the Wilks lambdas for 
# CCA data from the CCA library's method
# It computes the wilkes lambas the degrees of freedom and te 
# p-values
###################################################################

ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

#Wilk's Lambda Test
wilks = ccaWilks(attitude, health, cc)
round(wilks, 2)

# To understand the canonical correlates, we look at the raw coefficients
names(cc)
round(cc$xcoef, 3)  
round(cc$ycoef, 3)

# To help us better understand the components, we look at the correlations
# between each of the variates and the original variables that make them up.
loadings = comput(attitude, health, cc)
ls(loadings)
#Correlation X Scores
loadings$corr.X.xscores
#Correlation Y Scores
loadings$corr.Y.yscores

# Let's plot the first two variates against each other. We do this by looking at the scores.
plot(loadings$xscores[,1], loadings$yscores[,1])
cor(loadings$xscores[,1], loadings$yscores[,1])

# Last we look at the relationship between the variables and the correlates from the other dataset
# This gives us a better view of how the variables from one set relate to the other correlate and 
# how we might predict one from the other
loadings$corr.X.yscores   # How do the y-variates depend on the x-variables.  Most important for prediction
loadings$corr.Y.xscores  

# Problem 3
sports = read.csv(file = "sports.csv", header = TRUE)
head(sports)

# Convert the data as a table
dt <- as.table(as.matrix(sports))
dt <- mapply(dt[,c(2:6)], FUN = as.numeric)
dt <- matrix(data = dt, ncol =5, nrow = 5)
dt

chisq <- chisq.test(sports[,c(2:6)])
chisq

# Correspondence Analysis
library(ca)

prop.table(dt, 1)  # Row Percentages
prop.table(dt, 2)  # Col Percentages

fit <- ca(dt)
print(fit) # basic results 

plot(fit) # symmetric map
plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

library(vcd)
mosaic(dt, shade=TRUE, legend=TRUE,main = "Countries Mosaic Plot",
       xlab = "Countries",
       ylab = "Sports liking")

library(FactoMineR)
CA(dt, ncp = 5, graph = TRUE)
res.ca <- CA(sports[2:6], graph = FALSE)
print(res.ca)
# repel= TRUE to avoid text overlapping (slow if many point)
library("factoextra")
fviz_ca_biplot(res.ca, repel = TRUE)