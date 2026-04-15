# packages -------------------------------------------------
library(caret) #training and testing sets 
library(Metrics) #evaluate model metrics 
library(rsm)
library(factoextra)

# GLM model -------------------------------------------------

# add dimensions to mca dataframe 
mca_bin_pred <- mca_bin_pred %>%
  mutate(dim.1 = mca_bin_model$ind$coord[,1],
         dim.2 = mca_bin_model$ind$coord[,2],
         dim.3 = mca_bin_model$ind$coord[,3],
         dim.4 = mca_bin_model$ind$coord[,4])

## train test split of dataset ------------------------------
set.seed(1234)
train_idx <- createDataPartition(mca_bin_pred$SMS_freq_avg, p = 0.8, list = FALSE)

glm.train <- mca_bin_pred[train_idx,] #training data 
glm.test <- mca_bin_pred[-train_idx,] #testing data 

## Build GLM model ---------------------------------------

model.1 <- glm(SMS_freq_avg ~ dim.1 + dim.2 + dim.3 + dim.4,
               data = glm.train,
               family = gaussian())
summary(model.1)

modelChi <- model.1$null.deviance - model.1$deviance
chidf <- model.1$df.null - model.1$df.residual

pchisq(modelChi, chidf, lower.tail = FALSE)
#significant p-value for chi-sq test of deviance, glm model is an improvement over null model

## prediction and validation -----------------------------------
pred_glm <- predict(model.1, glm.test)

eval_metrics <- function(true, pred) {
  c(
    RMSE = rmse(true, pred),
    MAE = mae(true, pred),
    R2 = cor(true, pred)^2
  )
}

glm.perf <- eval_metrics(glm.test$SMS_freq_avg, pred_glm)
print(glm.perf)
# RMSE = 1.62
# MAE = 1.127
# R2 = 0.03
# not good metrics? 