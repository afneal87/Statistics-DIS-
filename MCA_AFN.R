# Packages ------------------------------------------------------------
library(factoextra)
library(haven) #read .sav files
library(dplyr) #data cleaning and manipulation
library(tidyverse) #data cleaning and manipulation
library(sjlabelled) #get variable labels 
library(psych) #descriptive statistics
library(FactoMineR) #MCA function
library(paletteer) #color palette 'MoMAColors::Klein'


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

hist(study2_clean$IMS_sat)

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

#correlation between IMS satisfaction and commitment scores 
cor(study2_clean$IMS_sat, study2_clean$IMS_com) # r = .80 
#correlation between positive and negative affect 
cor(study2_clean$positive_affect, study2_clean$negative_affect) #r = -0.29

#visual representation of bin distributions 
plot(study2_clean$SWLS_bin) #majority of respondents in bin 2
plot(study2_clean$SMS_visibility_bin) #majority of respondents in bin 2
plot(study2_clean$IMS_sat_bin) #majority of respondents in bin 3
plot(study2_clean$IMS_com_bin) #majority of respondents in bin 3
plot(study2_clean$IMS_total_bin) #majority of respondents in bin 3
plot(study2_clean$positive_affect_bin) #majority of respondents in bin 2
plot(study2_clean$negative_affect_bin) #majority of respondents in bin 2, higher proportion in bin 1
plot(study2_clean$esteem_bin) #majority of respondents in bins 2 and 3
plot(study2_clean$rel_understanding_bin) #majority of respondents in bin 3

# isolate variables of interest for binning model 
mca_bin_pred <- study2_clean %>%
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
    esteem_bin, #self-esteem score 
    rel_understanding_bin, #relationship understanding average 
    SMS_freq_avg #SMS frequency variable
  ) %>%
  na.omit()

### build MCA model with new predictor variables -------------------------

mca_bin_model <- MCA(mca_bin_pred, quanti.sup = 12, graph = TRUE)
summary(mca_bin_model, nbelements = Inf)
fviz_mca_var(mca_bin_model)

# scree plot of dimensions 
fviz_screeplot(mca_bin_model)
# higher percentage of variance explained by binned variables than original variables 
# scree plot suggests 3 or 4 dimensions are appropriate 
# model with IMS composite score suggests 3 dimensions are appropriate 

# see variable contributions for top 4 dimensions 
for (dim in 1:4) {
  plot_title <- paste('Contribution of Variables to Dimension', dim) 
  
  #create contribution plot for dimension 
  p <- fviz_contrib(mca_bin_model, choice = 'var', top = 20, axes = c(dim), gradient.cols = c('blue', 'white', 'red')) +
    ggtitle(plot_title)
  
  #display plots 
  print(p)
}

# additional visualizations 

fviz_mca_ind(mca_bin_model,
             label = 'none',
             habillage = 'p_gender',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(3,4)) +
  scale_color_paletteer_d('MoMAColors::Klein')
#ellipse shows confidence intervals. If ellipses do not overlap, groups are significantly different


dimdesc(mca_bin_model, axes = c(1:4))

#variable importance dimensions 1 and 2
fviz_mca_var(mca_bin_model, choice = 'var', axes = c(1,2))
# most important variables are ims score, positive affect, relationship understanding, 
# satisfaction with life, self esteem, and positive affect 

#individuals dimensions 1 and 2
fviz_mca_ind(mca_bin_model,
             label = 'none',
             habillage = 'IMS_total_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2)) +
  scale_color_paletteer_d('MoMAColors::Klein')

fviz_mca_ind(mca_bin_model,
             label = 'none',
             habillage = 'esteem_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2)) +
  scale_color_paletteer_d('MoMAColors::Klein')

fviz_mca_ind(mca_bin_model,
             label = 'none',
             habillage = 'SWLS_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2)) +
  scale_color_paletteer_d('MoMAColors::Klein')

fviz_mca_ind(mca_bin_model,
             label = 'none',
             habillage = 'rel_understanding_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2)) +
  scale_color_paletteer_d('MoMAColors::Klein')

fviz_mca_ind(mca_bin_model,
             label = 'none',
             habillage = 'negative_affect_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2)) +
  scale_color_paletteer_d('MoMAColors::Klein')

#variable importance dimensions 2 and 3
fviz_mca_var(mca_bin_model, choice = 'var', axes = c(2,3))
#no overlap in important variables 

#variable importance dimensions 1 and 3
fviz_mca_var(mca_bin_model, choice = 'var', axes = c(1,3))
# no overlap in important variables 

#variable importance dimensions 3 and 4
fviz_mca_var(mca_bin_model, choice = 'var', axes = c(3,4))
# overlapping variables are gender and sexual orientation

# MCA Model to predict relationship satisfaction -------------------------------

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

## build MCA model --------------------------------------------------

mca_model.3 <- MCA(mca_pred_esteem, quanti.sup = 11, graph = TRUE)
fviz_screeplot(mca_model.3)
#scree plot suggests 3 dimensions explain significant variation in data 

# see variable importance for first 2 dimensions
fviz_mca_var(mca_model.3, choice = 'var', axes = c(1,2))
#same variable importance as previous model but without self esteem categories 

# see variable importance for dimensions 3 and 4 
fviz_mca_var(mca_model.3, choice = 'var', axes = c(3,4))
#same variable importance as previous model

#see individuals in dimensions 1 and 2
fviz_mca_ind(mca_model.3,
             label = 'none',
             habillage = 'IMS_total_bin',
             addEllipses = TRUE, ellipse.type = 'confidence',
             ggtheme = theme_minimal(),
             axes = c(1,2)) +
  scale_color_paletteer_d('MoMAColors::Klein')
#same spread of individuals as previously 