# packages -------------------------------------------------
library(caret) #training and testing sets 
library(Metrics) #evaluate model metrics 
library(rsm)
library(factoextra)

# GLM model to predict social media frequency -------------------------------------------------

# add dimensions to mca dataframe 
mca_bin_pred <- mca_bin_pred %>%
  mutate(dim.1 = mca_bin_model$ind$coord[,1],
         dim.2 = mca_bin_model$ind$coord[,2],
         dim.3 = mca_bin_model$ind$coord[,3],
         dim.4 = mca_bin_model$ind$coord[,4])

par(mfrow=c(2,2))
hist(mca_bin_pred$dim.1)
hist(mca_bin_pred$dim.2)
hist(mca_bin_pred$dim.3)
hist(mca_bin_pred$dim.4)
dev.off()

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

# check for improvement by adding variables 
# try dropping dim 4 due to low contribution

model.2 <- glm(SMS_freq_avg ~ dim.1 + dim.2 + dim.3,
               data = glm.train,
               family = gaussian())
summary(model.2)

# run model metrics 
eval_metrics(glm.test$SMS_freq_avg, pred_glm)
#dropping dimension 4 does not improve the metrics of the model 

# GLM to predict self-esteem with new dimensions ------------------------------

# add dimensions to dataframe 
mca_pred_esteem <- mca_pred_esteem %>%
  mutate(dim.1 = mca_model.3$ind$coord[,1],
         dim.2 = mca_model.3$ind$coord[,2],
         dim.3 = mca_model.3$ind$coord[,3],
         dim.4 = mca_model.3$ind$coord[,4])

# split training and testing sets 
train_idx <- createDataPartition(mca_pred_esteem$esteem_avg, p = 0.8, list = FALSE)

glm.train <- mca_pred_esteem[train_idx,] #training data 
glm.test <- mca_pred_esteem[-train_idx,] #testing data 

# build GLM model 
glm.model.3 <- glm(esteem_avg ~ dim.1 + dim.2 + dim.3,
                   data = glm.train,
                   family = gaussian())
summary(glm.model.3)

pchisq(q = 1017.3-588.2, 
       df = 726 - 723,
       lower.tail = FALSE)
#significant p-value, new model is improvement over null model 

# make predictions with new model 
pred.esteem <- predict(glm.model.3, glm.test)

eval_metrics(glm.test$esteem_avg, pred.esteem)
#way better prediction 