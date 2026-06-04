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

## participant self-esteem --------------------------------

describe(study3_mca$SelfEsteem_avg)
# mean and median fall on higher end of the scale 

hist(study3_mca$SelfEsteem_avg)
# skewed to the left, sample shows high self esteem on average 

boxplot(study3_mca$SelfEsteem_avg)
# boxplot confirms skewed sample, some outliers flagged at the low end of the scale 

## satisfaction with life -------------------------------------------

describe(study3_mca$SWLS_avg)
# mean and media fall at higher end of the scale 

hist(study3_mca$SWLS_avg)
# slight skew to the left, higher proportion of high SWLS within sample 

boxplot(study3$SWLS_avg)
# boxplot confirms skewed distribution, some outliers flagged at low end of the scale 

## Number of social media profiles ------------------------------

describe(study3_mca$SMS_profiles)
# average number of profiles 5.5, median is 5 

hist(study3_mca$SMS_profiles)
# heavily skewed to the right, majority of participants report between 0 and 5 profiles 

boxplot(study3_mca$SMS_profiles)
# heavily skewed sample. Many outliers flagged on both extremes 

## Active and passive social media use ------------------------------

describe(select(study3_mca, SMS_active_passive_1, SMS_active_passive_2))
# higher average passive use than active use, higher median passive use than active use 

hist(study3_mca$SMS_active_passive_1)
# majority of respondents report level 2 active use 

hist(study3_mca$SMS_active_passive_2)
# majority of respondents report high passive use (levels 4 and 5) 

## online relationship visibility --------------------------------

describe(study3_mca$relation_visibility)
# average visibility of 2.31, media visibility 2 

hist(study3_mca$relation_visibility)
# majority of respondents report level 2 visibility ('somewhat, people who follow me know I have a partner') 

# Bin predictors and isolate MCA variables ------------------------------

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

# isolate only variables for MCA model 

s3_mca_vars <- study3_mca %>%
  select(
    p_gender, #participant gender 
    p_sexual_orientation, #participant sexual orientation
    IMS_sat_bin, #investment model satisfaction average 
    IMS_com_bin, #investment model commitment average 
    IMS_alt_bin, #investment model alternatives average 
    IMS_inv_bin, #investment model investment average 
    SMS_profiles_bin, #social media profiles 
    SMS_active_passive_1, #active social media use 
    SMS_active_passive_2, #passive social media use 
    SWLS_bin, #satisfaction with life 
    SelfEsteem_avg, #self-esteem score 
  ) %>%
  na.omit() %>%
  mutate(across(c(p_gender:SWLS_bin), as.factor))



# Multiple Correspondence Analysis ------------------------------------

s3_mca <- MCA(s3_mca_vars, quanti.sup = 11, graph = FALSE)

# scree plot of dimensions 
fviz_screeplot(s3_mca)
# dimension 1 contributes about 10% of variance to the sample, elbow bends about dimension 4 

# important variables for dimension 1 
fviz_contrib(s3_mca, choice = 'var', top = 20, axes = 1)
# top contributions are relationship measures, 8/10 top variables are IMS categories, other two are SWLS 

# important variables for dimension 2 
fviz_contrib(s3_mca, choice = 'var', top = 20, axes = 2)
# top contributing variables are IMS categories, 7th variable is SWLS 

# important variables for dimension 3
fviz_contrib(s3_mca, choice = 'var', top = 20, axes = 3)
# top contributing variables are social media use measures, active and passive 

# important variables for dimension 4 
fviz_contrib(s3_mca, choice = 'var', top = 20, axes = 4)
# top contributing variables are social media use measures 

## Important variables for dimensions 1 and 2  -----------------------------

fviz_mca_var(s3_mca, choice = 'var', axes = c(1,2))
# three most important variables to both dimensions are IMS sat, com, and inv
# other important dimensions are IMS alt and SWLS 

# IMS satisfaction on dimensions 1 and 2 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'IMS_sat_bin',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2))
# overlap in all three ellipses indicates no significant differences between satisfaction groups 

# IMS commitment on dimensions 1 and 2 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'IMS_com_bin',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2))
# no significant differences between low and mid 
# no significant differences between mid and high 
# significant difference between high and low 
# low score below zero on dimension 2 and above zero on dimension 1
# high cluster around the origin of both dimensions 

# IMS investment on dimensions 1 and 2 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'IMS_inv_bin',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2))
# significant difference between high and low 
# low score above 0 on dim 1, below 0 on dim 2
# high cluster around the origin of both dims 
# no significant differences between mid and either extreme 

# IMS alternatives on dims 1 and 2 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'IMS_alt_bin',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2))
# no significant differences across the three groups 

# SWLS on dims 1 and 2 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'SWLS_bin',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2))
# no significant differences across the three groups 

## Dimensions 3 and 4 -------------------------------------------

fviz_mca_var(s3_mca, choice = 'var', axes = c(3,4))
# most important variables for both dimensions are active and passive social media use 

# active social media use on dims 3 and 4 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'SMS_active_passive_1',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(3,4))
# no significant differences across all groups, all ellipses overlap 

# passive social media use on dims 3 and 4 
fviz_mca_ind(s3_mca, 
             label = 'none',
             habillage = 'SMS_active_passive_2',
             addEllipses = TRUE, elipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(3,4))
# no significant differences across all groups, all ellipses overlap 

