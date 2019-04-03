######## Analysis of differences

## data recode
library('data.table')
library('memisc')
library('dplyr')
library('stringr')
setwd('/well/nichols/users/bwj567')


ukbdata = fread('data/ukb25120_weighting_base.csv')
ukbdata <- as_tibble(ukbdata)

censusdata = fread('data/census11_recoded.csv')
censusdata <- as_tibble(censusdata)

hsedata = fread('data/hse16_recoded.csv')
hsedata <- as_tibble(hsedata)

# limit to people 40 to 70
censusdata <- censusdata %>% filter(demo_age_bucket >= '40 to 44') %>% filter(demo_age_bucket <= '65 to 69')

# limit HSE people to 40 to 70
hsedata <- hsedata %>% filter(demo_age_bucket >= '40 to 44') %>% filter(demo_age_bucket <= '65 to 69')


ukbdata %>% group_by(base_demo_age_bucket) %>% tally()
censusdata %>% group_by(demo_age_bucket) %>% tally()
hsedata %>% group_by(demo_age_bucket) %>% tally()

#summary function
getDemoSummary <- function(data, var){

	summary = data %>% 
                    group_by_(var) %>% 
                    summarize(count = n()
                        , dist = n()/nrow(data)
                        )

    cbind(var, summary)
}

getAllSummaries <- function(data, varlist, suffix = ""){

	# get summary
	summary = rbindlist(lapply(varlist, getDemoSummary, data = data))

	#set colnames
	suffix = ifelse(suffix == "", "", paste0("_", suffix))
	setnames(summary, c('var', 'level', paste0('count', suffix), paste0('dist', suffix)))

	# fix varnames
	summary[, var := gsub('base_', '', var)]

	return(summary)
}


# set list of vars
varlist = c(names(ukbdata)[grepl('base_demo', names(ukbdata))], names(ukbdata)[grepl('base_health', names(ukbdata))])


#### DO UKB SUMMARY
summary_base = getAllSummaries(data = ukbdata, varlist = varlist, suffix = 'ukb')

#### DO IMG SUMMARY
summary_img = getAllSummaries(data = ukbdata %>% filter(img_has_t1_MRI == 1), varlist = varlist, suffix = 'ukb_img')

#### DO CENSUS SUMMARY
summary_census = getAllSummaries(data = censusdata, varlist = names(censusdata)[grepl('demo|health', names(censusdata))], suffix = 'census')

#### DO HSE SUMMARY
summary_hse = getAllSummaries(data = hsedata, varlist = names(hsedata)[grepl('demo|health', names(hsedata))], suffix = 'hse16')


#### MERGE ALL TOGETHER
summary_full = Reduce(function(x, y) merge(x, y, by=c("var", 'level'), all = T), list(summary_img, summary_base, summary_census, summary_hse))

#### Rearrage columns
col_order = c('var', 'level'
	, names(summary_full)[grepl('count', names(summary_full))]
	, names(summary_full)[grepl('dist', names(summary_full))]
	)
summary_full = summary_full[, col_order, with = F]


#### Calc differences
summary_full[, diff_ukb := dist_ukb_img - dist_ukb]
summary_full[, diff_census := dist_ukb_img - dist_census]
summary_full[, diff_hse := dist_ukb_img - dist_hse16]

##### Write out to file
write.csv(summary_full, file = 'mini-project-1/analysis/summary_full.csv', row.names = F)



