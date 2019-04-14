
#############
# FUNCTIONS #
#############


#summary function
getDemoSummary <- function(data, var, weight.col = NULL){

	if(is.null(weight.col)){
		summary = data %>% 
                    group_by_(var) %>% 
                    summarize(count = n()
                        , dist = n()/nrow(data)
                        )
	}else{
		summary = data %>% 
					group_by_(var) %>%
					summarize(count = sum(get(weight.col), na.rm = T), dist = sum(get(weight.col), na.rm = T)/sum(data %>% select_(., weight.col), na.rm = T))
	}
    
    cbind(var, summary)
}

getAllSummaries <- function(data, varlist, suffix = "", weight.col = NULL){

    # get summary
    summary = rbindlist(lapply(varlist, getDemoSummary, data = data, weight.col = weight.col))

    #set colnames
    suffix = ifelse(suffix == "", "", paste0("_", suffix))
    setnames(summary, c('var', 'level', paste0('count', suffix), paste0('dist', suffix)))

    # fix varnames
    summary[, var := gsub('base_', '', var)]

    return(summary)
}
