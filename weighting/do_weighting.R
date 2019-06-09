########################
# Weighting neuro data #
########################

setwd('/well/nichols/users/bwj567/weighting')

# source weighting functions
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')

# load data
list.files('../data')

data_weighting = fread('../data/ukb25120_weighting.csv')

str(data_weighting)