###### SUMMARY of UKB Variables 


library(stringr)
library(knitr)
library(randomForest)
library(BayesTree)


setwd('/well/nichols/users/bwj567')
source('mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

###### PREP DATA

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv', stringsAsFactors = F)

setnames(ukbdata
    , old = names(ukbdata)[grepl('^img', names(ukbdata))]
    , new = gsub('^img_','',names(ukbdata)[grepl('^img', names(ukbdata))])
    )

#mean impute missing BMIs
ukbdata[is.na(bmi), bmi := mean(ukbdata$bmi, na.rm = T)]

# make missing vals DNK
ukbdata[is.na(demo_hh_size), demo_hh_size := '99-DNK/Refused']

ukbdata[, age_sq := age^2]
ukbdata[, bmi_sq := bmi^2]
ukbdata[, health_alc_weekly_total_sq := health_alc_weekly_total^2]

ukbdata$noise <- rnorm(nrow(ukbdata),0,1)

# check NAs
nas = apply(ukbdata, 2, function(x) sum(is.na(x)))
nas[nas > 0]


####### RUN SUMMARY
vars_to_consider = names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white|demo_educ_highest$', names(ukbdata))]

#summarize vars
varsum = apply(ukbdata[, vars_to_consider, with = F], 2, function(x) {
    if(length(unique(x)) < 20) {
        paste0(sort(unique(x)), collapse = ', ')
    } else {
        paste0(x[1:5], collapse = ', ')
    }
    })
head(varsum)
varsum = data.frame(variable = names(varsum), levels = varsum)
rownames(varsum) = NULL
kable(varsum, format = 'latex')
