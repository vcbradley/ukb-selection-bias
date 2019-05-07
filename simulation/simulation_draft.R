#### SIMULATION

library(stringr)

setwd('/well/nichols/users/bwj567')
source('mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv')
ukbdata <- as_tibble(ukbdata)

# drop img prefix
ukbdata <- ukbdata %>% rename_at(vars(starts_with("img_")), funs(str_replace(., "img_", "")))


#### Probability of missingness
names(ukbdata)

ukbdata

missingness_vars = c('demo_sex'
	, 'age' #continuous will be harder for weighting methods to deal with
	, 'demo_ethnicity_4way' #same with 4-way ethnicity v. two-way
	, 'demo_employed'
	, 'demo_income_bucket'
	, 'demo_educ_college_plus'
	, 'demo_empl_employed'
	)





# can we accurately identify variables related to missingness?

