## ---- LOGIT PCR--------------------------------------------------------
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