# packages -----------------------------------------------------
library(factoextra)
library(haven) #read .sav files
library(dplyr) #data cleaning and manipulation
library(tidyverse) #data manipulation
library(sjlabelled) #read variable labels
library(psych) #descriptive statistics
library(FactoMineR) #MCA function
library(paletteer) #color palette
library(caret) #training and testing sets 
library(codebook) #see codebook of variables 
library(ggiraph) #interactive plots

# Load data and clean for analysis -----------------------------------------

study3 <- read_sav('Data/Study3.sav')

#confirm all observations completed survey
table(study3$Finished)
# 6 observations unfinished, remove from dataset 

study3_fin <- study3 %>%
  filter(Finished == 1) %>% # keep only observations that completed full survey
  filter(SMS_profiles < 2000) # removed outlier observation and NAs (4) 

# factor categorical variables with labels 
study3_fin <- study3_fin %>%
  mutate(p_gender = factor(p_gender, labels = c('Man','Woman', 'Nonbinary','Not listed')), 
         p_sexual_orientation = factor(p_sexual_orientation, labels = c('Heterosexual', 'Bisexual', 'Lesbian/Gay', 'Pansexual', 'Asexual', 'Other')),
         )

# isolate variables for MCA, same as Study 2 
study3_mca <- study3_fin %>%
  select(p_age, # participant age 
         p_gender, # participant gender
         p_sexual_orientation, # participant sexual orientation
         p_SES, # participant socioeconomic status
         IMS_sat_avg, # investment model scale: satisfaction
         IMS_com_avg, # investment model scale: commitment
         IMS_alt_avg, # investment model scale: quality of alternatives
         IMS_inv_avg, # investment model scale: investment
         SelfEsteem_avg, # participant self-esteem
         SWLS_avg, # satisfaction with life 
         SMS_profiles, # number of social media profiles 
         SMS_active_passive_1, # active social media use 
         SMS_active_passive_2, # passive social media use 
         relation_visibility) # online relationship visibility 

# bin continuous variables into categories 
study3_mca <- study3_mca %>%
  mutate(
    # satisfaction with life scale 
    SWLS_bin = cut(SWLS_avg, breaks = c(0, 2.33, 4.66, 7), labels = c('low','mid','high')),
    # IMS satisfaction 
    IMS_sat_bin = cut(IMS_sat_avg, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    # IMS commitment
    IMS_com_bin = cut(IMS_com_avg, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    # IMS quality of alternatives
    IMS_alt_bin = cut(IMS_alt_avg, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    # IMS investment 
    IMS_inv_bin = cut(IMS_inv_avg, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    # number of social media profiles 
    SMS_profiles_bin = cut(SMS_profiles, breaks = c(0, 10, 20, Inf), labels = c('average', 'above average', 'high'))
  )

# Exploratory data analysis ---------------------------------------------------

## participant gender ----------------------------

table(study3_mca$p_gender)

plot(study3_mca$p_gender)
# sample is majority women 

## participant age -----------------------

describe(study3_mca$p_age)
# average age 38.98, median age 37 

hist(study3_mca$p_age)
# distribution looks approximately normal with majority observations in 30-40 range 

boxplot(study3_mca$p_age)
# boxplot suggests some skew toward younger ages and identifies one potential outlier over age 70 
# maximum age is 73 

## participant sexual orientation ---------------------------------

table(study3_mca$p_sexual_orientation)

plot(study3_mca$p_sexual_orientation)
# sample is majority heterosexual 

## participant socioeconomic status ------------------------------

table(study3_mca$p_SES)

hist(study3_mca$p_SES)
# sample is approximately normal

boxplot(study3_mca$p_SES)
# boxplot confirms sample is approximately normal 

## Investment model scale averages ----------------------------------

describe(select(study3_mca, IMS_sat_avg, IMS_com_avg, IMS_alt_avg, IMS_inv_avg))
# average scores suggest high relationship quality across the sample 

hist(study3_mca$IMS_sat_avg)
# satisfaction scores are heavily skewed to the left, high relationship satisfaction across sample 

hist(study3_mca$IMS_com_avg)
# highly skewed to the left, high relationship commitment across sample 

hist(study3_mca$IMS_alt_avg)
# more variation than sat and com but still skewed to the right, low quality of alternatives across sample 

hist(study3_mca$IMS_inv_avg)
# highly skewed to the left, high levels of investment across sample 

