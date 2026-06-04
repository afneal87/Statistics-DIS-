# packages ----------------------------------------------
library(factoextra)
library(haven) #read .sav files
library(dplyr) #data cleaning and manipulation
library(tidyverse) #data manipulation
library(sjlabelled) #read variable labels
library(psych) #descriptive statistics
library(FactoMineR) #MCA function
library(paletteer) #color palette
library(caret) #training and testing sets 

# Load in data and clean for analysis ----------------------------------

study1 <- read_sav('Data/Study1.sav')

# need to decide if using full dataset or only Ps in a relationship
# to match variables from study 2 analysis need to use only Ps in a relationship
