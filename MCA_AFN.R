# Packages ------------------------------------------------------------
library(factoextra)
library(haven) #read .sav files
library(dplyr) #data cleaning and manipulation
library(tidyverse) #data cleaning and manipulation
library(sjlabelled) #get variable labels 
library(psych) #descriptive statistics
library(FactoMineR) #MCA function


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
    rel_understanding_1:rel_understanding_5, #relative understanding by relationship partner
    SMS_plat_freq_1:SMS_plat_freq_10 #frequency of use of social media sites 
  )

# calculate social media frequency composite score based on the frequency 
# of use across all 10 platforms in the survey 
study2_clean <- study2_clean %>%
  mutate(SMS_freq_avg = rowMeans(pick(SMS_plat_freq_1:SMS_plat_freq_10), na.rm = TRUE))

# MCA Analysis ----------------------------------------------------------------

# isolate variables for MCA analysis 
mca_predictors <- study2_clean %>% 
  # select predictor variables 
  select(p_age:rel_understanding_5) %>% 
  #for every variable that is numberic, if the number of unique values 
  #is less than 10, make it a factor 
  mutate(across(where(is.numeric), 
                ~ if(n_distinct(.) <=10) as.factor(.) else .)) %>% 
  #select variables that are factors 
  select_if(is.factor) %>%
  # unsure what these lines do, ask Dr. Ghosh 
  select_if(function(x) n_distinct(x) > 1) %>%
  select_if(function(x) n_distinct(x) < 0.9 * nrow(study2_clean)) %>%
  na.omit()

## Build MCA model -------------------------------------------------

mca_model <- MCA(mca_predictors, graph = TRUE)
summary.MCA(mca_model)
plot(mca_model, invisible = 'ind', selectMod = 'contrib20')


## see variables contributing to dimensions --------------------

# scree plot 
fviz_screeplot(mca_model)
# based on the scree plot, four dimensions are the most appropriate to
# describe this data set 

# see variable contributions for top four dimensions 
for (dim in 1:4) {
  plot_title <- paste('Contribution of Variables to Dimension', dim) 
  
  #create contribution plot for dimension 
  p <- fviz_contrib(mca_model, choice = 'var', top = 25, axes = c(dim), gradient.cols = c('blue', 'white', 'red')) +
    ggtitle(plot_title)
  
  #display plots 
  print(p)
}

# all dimensions have 90 or more "important variables" contributing to them 

# each scale item consists of so many categories, it may be easier to 
# interpret results if scales are binned into larger categories. 


## Bin variables into larger groups for easier interpretability ---------------

