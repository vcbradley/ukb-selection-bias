#############
# Weighting #
#############



library('data.table')
library('memisc')
library('dplyr')
library('stringr')
setwd('/well/nichols/users/bwj567')


ukbdata = fread('data/ukb25120_weighting_base.csv')
ukbdata <- as_tibble(ukbdata)


hsedata = fread('data/hse16_recoded.csv')
hsedata <- as_tibble(hsedata)