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