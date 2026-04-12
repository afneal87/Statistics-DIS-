# Packages ------------------------------------------------------------
library(haven) #read .sav files
library(dplyr) #data cleaning and manipulation
library(tidyverse) #data cleaning and manipulation
library(sjlabelled) #get variable labels 


# Read in and clean data -------------------------------------------

study2 <- read_sav('Data/Study2.sav')

# create relationship length variable 
study2 <- study2 %>%
  mutate(rel_length = case_when(
    # if relationship is less than one year, divid number of months by 12 for porportion of year
    length_months_1 >=1 ~ length_months_1/12,
    # if relationship is longer than one year, keep variable the same 
    length_years_1 >= 1 ~ length_years_1))

# create new dataframe with variables for analysis 
study2_clean <- study2 %>%
  select(
    p_age, #participant age
    p_gender, #participant gender
    p_SES, #MacArthur subjective social status scale 
    p_trans, #transgender identity (y/n)
    p_race, #particpant race 
    p_sexual_orientation, #participant sexual orientation
    rel_distance, #relationship long distance (y/n)
    rel_length, #relationship length in years 
    SWLS_1:SWLS_5, #satisfaction with life scale
    SMS_profiles, #number of individual social media profiles across all platforms
    SMS_visibility_1:SMS_visibility_5, #relationship visibility on social media
    IMS_daily_1:IMS_daily_6, #investment model scale satisfaction and commitment
    pos_neg_affect_1:pos_neg_affect_6, #positive and negative affect scale
    est_connect_1:est_connect_9, #self esteem scale 
    SMSdaily_active_1, #active social media use 
    SMSdaily_active_2,#passive social media use 
    rel_understanding_1:rel_understanding_2, #relative understanding by relationship partner
    SMS_plat_freq_1:SMS_plat_freq_10 #frequency of use of social media sites 
  )

# calculate social media frequency composite score based on the frequency 
# of use across all 10 platforms in the survey 
study2_clean <- study2_clean %>%
  mutate(SMS_freq_avg = rowMeans(pick(SMS_plat_freq_1:SMS_plat_freq_10), na.rm = TRUE))



