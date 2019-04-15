#############
# Weighting #
#############


library('data.table')
library('memisc')
library('dplyr')
library('stringr')
library(survey)

setwd('/well/nichols/users/bwj567')

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv')
ukbdata <- as_tibble(ukbdata)

# drop img prefix
ukbdata <- ukbdata %>% rename_at(vars(starts_with("img_")), funs(str_replace(., "img_", "")))

# read in HSE data
hsedata = fread('data/hse16_recoded.csv')
hsedata <- as_tibble(hsedata)
# limit HSe data to relevant age range
hsedata <- hsedata %>% filter(demo_age_bucket >= '45 to 49') %>% filter(demo_age_bucket <= '75 to 79')





strat_vars = c('demo_sex'
, 'demo_age_bucket'
#, 'demo_white'
#, 'demo_empl_employed'
#, 'demo_income_bucket'
#, 'demo_hh_ownrent'
#, 'demo_occupation'
)

# function to create popframe

getPopframe = function(data, vars, weight_col = NULL){
	popframe = data %>% 
			group_by_at(vars(vars)) %>% 
			summarize(
				n = n()
				, prop = n()/nrow(data)
				)

	if(!is.null(weight_col)){
		data = data %>% mutate(weight = .data[[weight_col]])

		weighted = data %>% 
			group_by_at(vars(vars)) %>% 
			summarize(
				n_wt = sum(weight, na.rm = T)
				, prop_wt = sum(weight, na.rm = T)/sum(data$weight, na.rm = T)
				)

		popframe = left_join(popframe, weighted, by = vars)
	}

	# formula = as.formula(paste0('~', paste(vars, collapse = '+')))

	# if(!is.null(weight_col)){
	# 	data = data %>% filter(!is.na(get(weight_col)) & get(weight_col) > 0)
	# 	weights = as.formula(paste0('~', weight_col))
	# }else{
	# 	weights = as.formula('~1')
	# }

	# svydata = svydesign(id=~1, weights = weights, data = data)

	# popframe = svytable(formula, svydata)

	popframe = popframe %>% mutate(Freq = get(ifelse(!is.null(weight_col), 'prop_wt', 'prop')))

	return(popframe %>% select(-c(n,prop,n_wt,prop_wt)))
}


#### Function to post-stratify survey
doPostStrat = function(svydata, popdata, vars, pop_weight_col = NULL){
	# get population frame
	popframe  = getPopframe(popdata, vars = vars, weight_col = pop_weight_col)

	# make survey data
	svydata = svydesign(id = ~1, weights = ~1, data = svydata)

	# define strata for post-stratification
	strata = as.formula(paste0('~', paste(vars, collapse = '+')))

	# weight
	weighted = postStratify(svydata, strata = strata, population = popframe)
	weighted = cbind(weighted$variables, weight = (1/weighted$prob)/mean(1/weighted$prob, na.rm = T))

	#check mean
	mean(weighted$weight)

	return(weighted)
}

ukbweighted = doPostStrat(svydata = ukbdata, popdata = hsedata, vars = strat_vars, pop_weight_col = 'wt_blood')

names(ukbweighted)


doRaking = function(svydata, popdata, vars, pop_weight_col = NULL, control = list(maxit = 100, epsilon = 10e-4, verbose=FALSE)){
	# get population frame
	popmargins = lapply(vars, getPopframe, data = popdata, weight_col = pop_weight_col)

	strata = lapply(vars, function(x) as.formula(paste("~", x)))

	svydata = svydesign(id = ~1, weights = ~1, data = svydata)

	# do weighting
	weighted = rake(svydata, sample.margins = strata, population.margins = popmargins, control = control)
	weighted = cbind(weighted$variables, weight = (1/weighted$prob)/mean(1/weighted$prob, na.rm = T))

	return(weighted)
}

ukbweighted = doRaking(svydata = ukbdata, popdata = hsedata, vars = strat_vars, pop_weight_col = 'wt_blood')

names(ukbweighted)