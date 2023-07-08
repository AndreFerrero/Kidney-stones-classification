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

## ---- PCA ----------------------------------------------------------
# It appears to be sensible to try a PCA and
# see whether we can reduce the number of dimensions
library(factoextra)

pca <- prcomp(Z)

# var cum with 2 pc is 76.97
summary(pca)


# screeplot suggests 2 pc
fviz_eig(pca)

# Individuals plot on 2 pc
# Many points are actually well represented
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# calcium is the variable that contributes the least to the total variance
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# since 2 pc explain a good variation of the total variance, a biplot might be useful
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# colour by target
target <- as.factor(data[ , 7])

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "green", # Variables color
                col.ind = target  # Individuals color
)
# it looks like people with kidney stones appear to have values distant from the mean,
# suggesting some relationship, even though not super clear from this representation
# It appears as well that some that weren't diagnosed positively had low ph values

# extract the first 2 pc
pc <- pca$x
pc2 <- pc[ , 1:2]

## ---- LOGISTIC --------------------------------------------------------
# create dataframe with the principal components
set.seed(1)

pc_df <- data.frame(PC1 = pc2[, 1], PC2 = pc2[, 2], target = target)
str(pc_df)

# create train and test sets for pcr df using dplyr
library(dplyr)

# add an id variable
pc_df$id <- 1 : nrow(pc_df)

train <- pc_df %>% sample_frac(0.7)
test <- anti_join(pc_df, train, by = "id")


# create train and test sets for the general data

data$id <- 1:nrow(data)

train_base <- data %>% sample_frac(0.7)

test_base <- anti_join(data, train_base, by = "id")

train_base <- train_base[ , -8]
test_base <- train_base[ , -8]



logit_pcr <- glm(target ~ PC1 + PC2, data = train, family = binomial(link = "logit"))
summary(logit_pcr)


logit_baseline <- glm(target ~ ., data = train_base, family = binomial(link = "logit"))
summary(logit_baseline)



# metrics for PCR
ypcr <- predict(logit_pcr, newdata = test[ , c("PC1", "PC2")], type = "response")
ypcr <- ifelse( ypcr > 0.5, "1", "0")

pcr <- data.frame(ypcr = ypcr, target = test$target)

# accuracy of logit_pcr
library(metrica)
# TP + TN / (TP+...+FN)
accuracy(data = pcr, obs = target, pred = ypcr)
# TP / (TP + FP)
precision(data = pcr, obs = target, pred = ypcr)
# TP / P
sensitivity(data = pcr, obs = target, pred = ypcr)


# metrics for baseline
ybase <- predict(logit_baseline, 
                 newdata = test_base[ , 1:6],
                 type = "response")

ybase <- ifelse( ybase > 0.5, "1", "0")

base <- data.frame(ybase = ybase, target = test_base$target)

# accuracy of logit_pcr
accuracy(data = base, obs = target, pred = ybase)
precision(data = base, obs = target, pred = ybase)
sensitivity(data = base, obs = target, pred = ybase)


## ----CLUSTERING-------------------
# are there natural differences between people? Or are they all the same?
# let's try clusering

# As a preliminary tool, let's use hierarchical clustering
# Usually the mean method provides the most robust results, so let's go with that first

library(cluster)
D <- dist(Z, method = "euclidean")

output_m = hclust(d = D, method = "average")

plot(output_m, main = "Average method",
     ylab = "Distanza", hang = -1, frame.plot = TRUE)

# It's kind of difficult to say, the groups join at quite high level of distances.

# Let's try the Ward method, that tries to group so that new groups have the lowest possible deviance within

output_w <- hclust(d = D, method = "ward.D2")

plot(output_w, main = "Ward method",
     ylab = "Distanza", hang = -1, frame.plot = TRUE)

plot(output_w, main = "Ward method",
     ylab = "Distanza", hang = -1, frame.plot = TRUE)
rect.hclust(output_w, k = 2, border="red")

# 10 looks okay, but it's difficult... Let's apply kmeans for different group numbers
# and see which g is the one that has the lowest decrease in the wss

# This function creates the scree plot for a given dataset data and nc groups
# The kmeans algorithm will have nstart = 50 to guarantee that an optimal wss is found at each run
wssplot <- function(data, nc=15, seed=1234){
  # nc = number of centers used
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    # grabs the wss from the kmeans object
    wss[i] <- sum(kmeans(data, centers=i, nstart = 50)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

# From the screeplot, 2 is the actual number of groups
# (maybe the Yes or No related to the target)
wssplot(Z)

km <- kmeans(Z, centers = 2, nstart = 50)

# As the PCA biplot coloured by the target kind of told us,
# one group has lower than the mean values for the covariates except for ph,
# which is higher than the mean

km$centers





