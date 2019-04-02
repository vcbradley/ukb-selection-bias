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

censusdata



#summary function
getDemoSummary <- function(data, var){
	
	if(exists('img_was_imaged', data)){
		n_imaged = sum(data$img_was_imaged, na.rm = T)

		summary = data %>% 
					group_by_(var) %>% 
					summarize(count_base = n()
						, count_img = sum(img_was_imaged, na.rm = T)
						, dist_base = n()/nrow(data)
						, dist_img = sum(img_was_imaged, na.rm = T)/n_imaged
						)
	} else {
		summary = data %>% 
					group_by_(var) %>% 
					summarize(count_census = n()
						, dist_census = n()/nrow(data)
						)
	}

	cbind(var, summary)
}


#### DO UKB SUMMARY
var_list = c(names(ukbdata)[grepl('base_demo', names(ukbdata))], names(ukbdata)[grepl('base_health', names(ukbdata))])
summary_base = rbindlist(lapply(var_list, getDemoSummary, data = ukbdata))
summary_base[, var := gsub('base_demo', 'demo', var)]
setnames(summary_base, old = 'base_demo_sex', new = 'level')

#### DO CENSUS SUMMARY
summary_census = rbindlist(lapply(names(censusdata)[grepl('demo_', names(censusdata))], getDemoSummary, data = censusdata))
setnames(summary_census, old = 'demo_sex', new = 'level')

merge(summary_base, summary_census, by = c('var', 'level'), all = T)

summary_base <- summary_base %>% mutate(dist_diff = dist_img - dist_base)



##### Write out to file
write.csv(summary_base, file = 'mini-project-1/summary_base.csv', row.names = F)


getDemoSummary(data = ukbdata, var = 'base_demo_sex')


