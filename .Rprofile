library(tidyverse)


setwd("C:/Users/andre/OneDrive - Università degli Studi di Torino/R/Kidneys")
data <- utils :: read.csv("kidney.csv")

# extract predictors
X <- data[ , -7]

# let's standardize the predictors since we don't know much about the biological problem
# and want everything to the same scale  
# This affects analysis that aren't invariant to scale transformations (e.g. PCA)
Z <- scale(X)
summary(Z)
