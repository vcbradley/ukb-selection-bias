######## Analysis of differences

## data recode
library('data.table')
library('memisc')
library('dplyr')
library('stringr')
setwd('/well/nichols/users/bwj567')

# source summary functions
source('mini-project-1/analysis/functions.R')


ukbdata = fread('data/ukb25120_weighting_base.csv')
ukbdata <- as_tibble(ukbdata)

ukbimgdata = fread('data/ukb25120_weighting_img.csv')
ukbimgdata <- as_tibble(ukbimgdata)

censusdata = fread('data/census11_recoded.csv')
censusdata <- as_tibble(censusdata)

hsedata = fread('data/hse16_recoded.csv')
hsedata <- as_tibble(hsedata)

hsedata11 = fread('data/hse11_recoded.csv')
hsedata11 <- as_tibble(hsedata11)

# limit to people 40 to 70
censusdata <- censusdata %>% filter(demo_age_bucket >= '40 to 44') %>% filter(demo_age_bucket <= '65 to 69')

# limit HSE people to 40 to 70
hsedata <- hsedata %>% filter(demo_age_bucket >= '40 to 44') %>% filter(demo_age_bucket <= '80 to 84')
hsedata11 <- hsedata11 %>% filter(demo_age_bucket >= '40 to 44') %>% filter(demo_age_bucket <= '65 to 69')


ukbdata %>% group_by(base_demo_age_bucket) %>% tally()
ukbimgdata %>% group_by(img_demo_age_bucket) %>% tally()
censusdata %>% group_by(demo_age_bucket) %>% tally()
hsedata %>% group_by(demo_age_bucket) %>% tally()
hsedata11 %>% group_by(demo_age_bucket) %>% tally()

hsedata %>% select(., grep('wt', names(hsedata)))
hsedata11 %>% select(., grep('wt', names(hsedata11)))


# BP = NURSE

#hsedata %>% group_by_('demo_age_bucket') %>% summarize(count_raw = n(), dist_raw = n()/nrow(hsedata), dist = sum(get(weight.col))/sum(hsedata %>% select_(., weight.col)))


# set list of vars
varlist_base = c(names(ukbdata)[grepl('base_demo', names(ukbdata))], names(ukbdata)[grepl('base_health', names(ukbdata))])
varlist_img = c(names(ukbimgdata)[grepl('img_demo', names(ukbimgdata))], names(ukbimgdata)[grepl('img_health', names(ukbimgdata))])

#### DO UKB SUMMARY
summary_base = getAllSummaries(data = ukbdata, varlist = varlist_base, suffix = 'ukb')

#### DO IMG SUMMARY
summary_img = getAllSummaries(data = ukbdata %>% filter(img_has_t1_MRI == 1), varlist = varlist_base, suffix = 'ukb_img')

#### DO IMG AT TIME OF IMAGING SUMMARY
summary_img_atimg = getAllSummaries(data = ukbimgdata %>% filter(img_has_t1_MRI == 1), varlist = varlist_img, suffix = 'ukb_img_atimg')

#### DO CENSUS SUMMARY
summary_census = getAllSummaries(data = censusdata, varlist = names(censusdata)[grepl('demo|health', names(censusdata))], suffix = 'census')

#### DO HSE SUMMARY
summary_hse = getAllSummaries(data = hsedata, varlist = names(hsedata)[grepl('demo|health', names(hsedata))], suffix = 'hse16', weight.col = 'wt_nurse')

#### DO HSE11 SUMMARY
summary_hse11 = getAllSummaries(data = hsedata11, varlist = names(hsedata11)[grepl('demo|health', names(hsedata11))], suffix = 'hse11', weight.col = 'wt_nurse')


#### MERGE ALL TOGETHER
summary_full = Reduce(function(x, y) merge(x, y, by=c("var", 'level'), all = T), list(summary_img, summary_base, summary_img_atimg, summary_census, summary_hse11, summary_hse))

#### Rearrage columns
col_order = c('var', 'level'
    , names(summary_full)[grepl('count', names(summary_full))]
    , names(summary_full)[grepl('dist', names(summary_full))]
    )
summary_full = summary_full[, col_order, with = F]



#### Calc differences
summary_full[, diff_ukb := dist_ukb_img - dist_ukb]
summary_full[, diff_census := dist_ukb_img - dist_census]
summary_full[, diff_hse11 := dist_ukb_img - dist_hse11]
summary_full[, diff_hse16 := dist_ukb_img_atimg - dist_hse16]


##### Write out to file
write.csv(summary_full[var != 'health_alc_weekly_total'], file = 'mini-project-1/analysis/summary_full.csv', row.names = F, na = "")



