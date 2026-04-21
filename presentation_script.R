# packages -----------------------------------------------
library(factoextra)
library(haven) #read .sav files
library(dplyr) #data cleaning and manipulation
library(tidyverse) #data cleaning and manipulation
library(sjlabelled) #get variable labels 
library(psych) #descriptive statistics
library(FactoMineR) #MCA function
library(paletteer) #color palette 'MoMAColors::Klein'

# Dataset  --------------------------------------------

study2 <- read_sav('Data/Study2.sav')

## clean data and factor variables ----------------------------

study2_clean <- study2 %>%
  select(
    p_age, #participant age
    p_gender, #participant gender
    p_SES, #MacArthur subjective social status scale 
    p_trans, #transgender identity (y/n)
    p_race, #particpant race 
    p_sexual_orientation, #participant sexual orientation
    rel_distance, #relationship long distance (y/n)
    SWLS_1:SWLS_5, #satisfaction with life scale
    SMS_profiles, #number of individual social media profiles across all platforms
    SMS_visibility_1:SMS_visibility_5, #relationship visibility on social media
    IMS_daily_1:IMS_daily_6, #investment model scale satisfaction and commitment
    pos_neg_affect_1:pos_neg_affect_6, #positive and negative affect scale
    est_connect_1:est_connect_9, #self esteem scale 
    SMSdaily_active_1, #active social media use 
    SMSdaily_active_2,#passive social media use 
    rel_understanding_1:rel_understanding_5, #relative understanding by relationship partner
  )

#factor demographic variables 
study2_clean <- study2_clean %>%
  mutate(
    p_gender = factor(p_gender, labels = c('Agender',
                                           'Man',
                                           'Woman',
                                           'Non-binary')),
    p_trans = factor(p_trans, labels = c('Transgender',
                                         'Cisgender')),
    p_sexual_orientation = factor(p_sexual_orientation, labels = c('Asexual',
                                                                   'Bisexual',
                                                                   'Heterosexual',
                                                                   'Lesbian or Gay',
                                                                   'Pansexual',
                                                                   'Not listed')),
    rel_distance = factor(rel_distance, labels = c('Long distance',
                                                   'No long distance'))
  )

# create composite scores from scales 
study2_clean <- study2_clean %>%
  mutate(
    # satisfaction with life scale 
    SWLS_avg = rowMeans(pick(SWLS_1:SWLS_5), na.rm = TRUE),
    # online relationship visibility
    SMS_visibility_4R = 8 - SMS_visibility_4,
    SMS_visibility_avg = rowMeans(pick(SMS_visibility_1:SMS_visibility_3, SMS_visibility_4R, SMS_visibility_5), na.rm = TRUE),
    # investment model scale satisfaction
    IMS_sat = rowMeans(pick(IMS_daily_1:IMS_daily_3), na.rm = TRUE),
    # investment model commitment scale 
    IMS_com = rowMeans(pick(IMS_daily_4:IMS_daily_5), na.rm = TRUE),
    # total investment model scale 
    IMS_total = rowMeans(pick(IMS_daily_1:IMS_daily_6), na.rm = TRUE),
    # positive affect total 
    positive_affect = rowMeans(pick(pos_neg_affect_1:pos_neg_affect_3), na.rm = TRUE),
    # negative affect total 
    negative_affect = rowMeans(pick(pos_neg_affect_4:pos_neg_affect_6), na.rm = TRUE),
    # self esteem
    est_connect_1R = 8 - est_connect_1,
    est_connect_4R = 8 - est_connect_4,
    est_connect_5R = 8 - est_connect_5,
    est_connect_8R = 8 - est_connect_5, 
    esteem_avg = rowMeans(pick(est_connect_2, est_connect_3, est_connect_6, est_connect_7, est_connect_1R:est_connect_8R), na.rm = TRUE),
    rel_understanding_avg = rowMeans(pick(rel_understanding_1:rel_understanding_5), na.rm = TRUE)
  )

#bin continuous variables into categories 
study2_clean <- study2_clean %>%
  mutate(
    # satisfaction with life scale 
    SWLS_bin = cut(SWLS_avg, breaks = c(0, 2.33, 4.66, 7), labels = c('low','mid','high')),
    # online relationship visibility
    SMS_visibility_bin = cut(SMS_visibility_avg, breaks = c(0, 2.33, 4.66, 7), labels = c('low','mid','high')),
    # IMS satisfaction 
    IMS_sat_bin = cut(IMS_sat, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    # IMS commitment
    IMS_com_bin = cut(IMS_sat, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    #IMS total 
    IMS_total_bin = cut(IMS_total, breaks = c(0, 3, 6, 9), labels = c('low','mid','high')),
    # positive affect 
    positive_affect_bin = cut(positive_affect, breaks = c(0, 2.33, 4.66, 7), labels = c('low','mid','high')),
    # negative affect
    negative_affect_bin = cut(negative_affect, breaks = c(0,2.33,4.66,7), labels = c('low','mid','high')),
    #self esteem 
    esteem_bin = cut(esteem_avg, breaks = c(0, 2.33, 4.66, 7), labels = c('low','mid','high')),
    #relationship understanding
    rel_understanding_bin = cut(rel_understanding_avg, breaks = c(0, 1.66, 3.33, 5), labels = c('low','mid','high'))
  )

# isolate variables of interest for binning model 
mca_pred_esteem <- study2_clean %>%
  select(
    p_gender, #participant gender
    p_trans, #participant trans identity (y/n)
    p_sexual_orientation, #participant sexual orientation
    rel_distance, #relationship long distance (y/n)
    SWLS_bin, #satisfaction with life average 
    SMS_visibility_bin, #online relationship visibility average
    IMS_total_bin, #investment model composite score
    positive_affect_bin, #positive affect/mood 
    negative_affect_bin, #negative affect/mood 
    rel_understanding_bin, #relationship understanding average 
    esteem_avg #self-esteem average 
  ) %>%
  na.omit()

# MCA model ---------------------------------------------------

mca_model.3 <- MCA(mca_pred_esteem, quanti.sup = 11, graph = FALSE)