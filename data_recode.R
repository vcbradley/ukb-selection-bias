## data recode
library('data.table')
setwd('/well/nichols/projects/UKB/SMS')

# set location of full data file
full_data_file = 'ukb25120.csv'

# read in data header to get all names
data_head = names(fread(full_data_file, nrows = 0))

# read in list of variables
variables = fread('/well/nichols/users/bwj567/mini-project-1/variable_codings.csv')
variables = unique(variables[, .(cat, var, `biobank code`)])
setnames(variables, old = 'biobank code', new = 'biobank_code')

all_parent_codes = variables[, biobank_code]

# grab list of actual variable names in the data (at baseline)
vars_baseline = lapply(all_parent_codes, function(c){
    data_head[grepl(paste0('^',c,'-0'), data_head)]
    })
names(vars_baseline) = variables[, var]
vars_baseline = unlist(vars_baseline)
vars_baseline = data.table(var = names(vars_baseline), code = vars_baseline)


# grab list of actual variable names in the data (at imaging)
vars_imaging = lapply(all_parent_codes, function(c){
    data_head[grepl(paste0('^',c,'-2'), data_head)]
    })
names(vars_imaging) = variables[, var]
vars_imaging = unlist(vars_imaging)
vars_imaging = data.table(var = names(vars_imaging), code = vars_imaging)



# read in ALL the imaging data
data_imaging = fread(full_data_file
    , nrows = 5000     #for testing
    , select = c('eid', vars_imaging$code)
    , col.names = c('eid', vars_imaging$var)
    , na.strings = ''
    )
data_imaging

# check imaging variables
data_imaging[, .N, is.na(brain_vol)]
data_imaging[, .N, .(has_brain_vol = !is.na(brain_vol), MRI_completed == '1')]
data_imaging[, .N, .(has_brain_vol = !is.na(brain_vol), MRI_completed == '1')]

data_imaging[, .N, assessment_center]



# read in ALL the imaging data
data_baseline = fread(full_data_file
    , nrows = 5000     #for testing
    , select = c('eid', vars_baseline$code)
    , col.names = c('eid', vars_baseline$var)
    , na.strings = ''
    )
data_baseline

data_baseline[, ]


###########
# RECODES #
###########

#



## VAR-INSTANCE.MEASUREMENT
data[, .(.N,.N/nrow(data)), by = `6138-0.0`][order(`6138-0.0`)]
data[, .(.N,.N/nrow(data)), by = `6138-0.1`][order(`6138-0.1`)]
data[, .(.N,.N/nrow(data)), by = `6138-0.2`][order(`6138-0.2`)]
data[, .(.N,.N/nrow(data)), by = `6138-0.3`][order(`6138-0.3`)]

data[, .(.N,.N/nrow(data)), by = .(`6138-0.0`, `6138-0.1`, `6138-0.2`)][order(`6138-0.0`)]



data = fread('ukb25120.csv'
	, nrows = 10000 	#for testing
	, select = c("132-0.0", "132-0.1","132-0.2","132-0.3","132-0.4","132-0.5")
	, col.names = c('job1', 'job2', 'job3', 'job4', 'job5', 'job6')
	)

data[, job1 := `132-0.0`]
data[, substr(job1, 1,1)]
data[job1 != '' & job1 != 0, .(.N), by = .(jobtop = substr(job1, 1,1))][order(jobtop)][, .(jobtop, N, N/sum(N))]
