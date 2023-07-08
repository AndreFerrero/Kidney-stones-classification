# The six physical characteristics of the urine are:
# (1) specific gravity, the density of the urine relative to water;
# (2) pH, the negative logarithm of the hydrogen ion;
# (3) osmolarity (mOsm), a unit used in biology and medicine but not in
# physical chemistry. Osmolarity is proportional to the concentration of
# molecules in solution;
# (4) conductivity (mMho milliMho). One Mho is one
# reciprocal Ohm. Conductivity is proportional to the concentration of charged
# ions in solution;
# (5) urea concentration in millimoles per litre;
# (6) calcium, concentration (CALC) in millimolesllitre.

## ---- VISUALS -------------------------------------------------------
# Histograms of predictors
par(mfrow = c(2, 3))
for(i in colnames(Z)){
  hist(Z[ , i], main = paste("Histogram of", i), xlab = NULL)
}

# Boxplots of predictors
for(i in colnames(Z)){
  boxplot(Z[ , i], main = paste("Histogram of", i), xlab = NULL)
}

# The target variable is not much unbalanced
par(mfrow = c(1,1))
barplot(table(data[ , 7]), main = "Barplot of target")

## ---- CORRELATION? -------------------------------------------------
library(GGally)
ggscatmat(Z)

# Gravity appears to be correlated highly with Osmolarity (0.86), Conductivity (0.56),
# highly with Urea concentration (0.82), and Calcium (0.53)

# Osmolarity appears to be correlated higly with Conductivity (0.81),
# highly with Urea concentration(0.87) and Calcium(0.52)

# Conductivity appears to be correlated with Urea concentration(0.5)

# Urea concentration appears to be correlated with calcium(0.5)







