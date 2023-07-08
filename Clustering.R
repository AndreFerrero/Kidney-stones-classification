## ----CLUSTERING-------------------
# are there natural differences between people? Or are they all the same?
# let's try clustering

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

# 2 looks good because they're quite distant...
# Let's apply kmeans for different group numbers
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

# As the PCA biplot coloured by the target told us,
# one group has lower than the mean values for the covariates except for ph,
# which is higher than the mean

km$centers

km$cluster

# extract the cluster binary variable and add it to the original df

data_cluster <- cbind(data, km$cluster)
names(data_cluster)[names(data_cluster) == "km$cluster"] <- "cluster"

# the second group does indeed have more points that have kidneys stones
# 20% more
data_cluster %>% group_by(cluster) %>%
  summarise(sum(target)/length(target))














