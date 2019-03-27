## data recode
library('data.table')
library('memisc')
library('dplyr')
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

data_baseline[, .N, .(year(assessment_date))][order(year)]


###########
# RECODES #
###########

# GENDER
data_baseline[, gender_male := as.numeric(sex)]
data_baseline[, demo_sex := ifelse(sex == 0, 'M', 'F')]

# AGE
data_baseline[, age]

# ethnicity
data_baseline[, demo_ethnicity := substr(ethnicity, 1,1)]
data_baseline[, .N, demo_ethnicity]
data_baseline[, demo_white := 'Non-white']
data_baseline[demo_ethnicity == 1, demo_white := 'White'

# employment status
doEmplRecode = function(r){
	employed = as.numeric(any(r == 1))
	retired = as.numeric(any(r == 2))
	homemaker = as.numeric(any(r == 3))
	disabled = as.numeric(any(r == 4))
	unemployed = as.numeric(any(r == 5))
	volunteer = as.numeric(any(r == 6))
	student = as.numeric(any(r == 7))

	return(list(employed = employed
		, retired = retired
		, homemaker = homemaker
		, disabled = disabled
		, unemployed = unemployed
		, volunteer = volunteer
		, student = student
		))
}

data_baseline = cbind(data_baseline
	, rbindlist(apply(data_baseline[, .(job_status1, job_status2, job_status3, job_status4, job_status5, job_status6, job_status7)]
		, 1, doEmplRecode)))

# check overlap
data_baseline[, .N, .(employed, retired, homemaker, disabled, unemployed, volunteer, student)]

# OCCUPATION
table(unlist(lapply(data_baseline[, job_code], length))) # all the same lemgth
data_baseline[, job_topcat := substr(job_code,1,1)]
data_baseline[, .N, .(is.na(job_topcat), employed)]
data_baseline[, .N, job_topcat]

data_baseline[, demo_occupation := '10-unemployed']
data_baseline[job_topcat == 0, demo_occupation := '00-military']
data_baseline[job_topcat == 1, demo_occupation := '01-manager']
data_baseline[job_topcat == 2, demo_occupation := '02-professional']
data_baseline[job_topcat == 3, demo_occupation := '03-assoc professional']
data_baseline[job_topcat == 4, demo_occupation := '04-admin']
data_baseline[job_topcat == 5, demo_occupation := '05-skilled trades']
data_baseline[job_topcat == 6, demo_occupation := '06-personal service']
data_baseline[job_topcat == 7, demo_occupation := '07-sales customer service']
data_baseline[job_topcat == 8, demo_occupation := '08-industrial']
data_baseline[job_topcat == 9, demo_occupation := '09-elementary']

data_baseline[, .N/nrow(data_baseline), demo_occupation]


# EDUCATION
# educ1 educ2 educ3 educ4 educ5 educ6
data_baseline[, educ_collegeplus := ifelse(educ1 == 1, 1, 0)]
data_baseline[, .N, educ_collegeplus]

#r = vector of education data
doEducRecode = function(r){
        educ_collegeplus = as.numeric(any(r == 1, na.rm = T))
        educ_alevels = as.numeric(any(r == 2, na.rm = T))
        educ_olevels = as.numeric(any(r == 3, na.rm = T))
        educ_cses = as.numeric(any(r == 4, na.rm = T))
        educ_vocational = as.numeric(any(r == 5, na.rm = T))
        educ_profesh = as.numeric(any(r == 6, na.rm = T))

        return(list(educ_collegeplus = educ_collegeplus
        	, educ_alevels = educ_alevels
        	, educ_olevels = educ_olevels
        	, educ_cses = educ_cses
        	, educ_vocational = educ_vocational
        	, educ_profesh = educ_profesh
        	))
        }

data_baseline = cbind(data_baseline
	, rbindlist(apply(data_baseline[, .(educ1, educ2, educ3, educ4, educ5, educ6)], 1, doEducRecode)))

data_baseline[, demo_educ_highest := cases(
            '01-College plus' = (educ1 == 1)
            , '02-A Levels' = (educ1 == 2)
            , '03-O Levels' = (educ1 == 3)
            , '04-CSEs' = (educ1 == 4)
            , '05-Vocational' = (educ1 == 5)
            , '06-Other professional' = (educ1 == 6)
            , '07-None' = TRUE
            )]
data_baseline[, .N, .(demo_educ_highest, educ1)][order(demo_educ_highest)]

        
# income
data_baseline[, .N, hh_income_cat]
data_baseline[, demo_income_bucket := cases(
	'01-Under 18k' = (hh_income_cat == 1)
	, '02-18k to 31k' = (hh_income_cat == 2)
	, '03-31k to 52k' = (hh_income_cat == 3)
	, '04-52k to 100k' = (hh_income_cat == 4)
	, '05-Over 100k' = (hh_income_cat == 5)
	, '06-DNK' = (hh_income_cat == -1)
	, '07-Refused' = TRUE
	)]
data_baseline[, .N, .(hh_income_cat, demo_income_bucket)][order(demo_income_bucket)]

# location
addr_dates = data_baseline[,vars_baseline[grepl('addr_firstdate', var), var][15:1], with = F]

# the most recent date is in the last populated field
sum(apply(addr_dates,1, function(r) {
	r[!is.na(r)][1]
	})==
apply(addr_dates,1, function(r) {
	max(r, na.rm = T)
	})
, na.rm = T)

#which means that we can apply the same logic to find most recent location
addr_east_all = data_baseline[, vars_baseline[grepl('addr_east', var), var][15:1], with = F]
data_baseline[, 'addr_east_recent' := apply(addr_east_all, 1, function(r){
	r[!is.na(r)][1]
	})]

addr_north_all = data_baseline[, vars_baseline[grepl('addr_north', var), var][15:1], with = F]
data_baseline[, 'addr_north_recent' := apply(addr_north_all, 1, function(r){
	r[!is.na(r)][1]
	})]

#some checks that it worked
data_baseline[is.na(addr_east2), sum(addr_east1 == addr_east_recent, na.rm = T)/sum(is.na(addr_east2))]
data_baseline[is.na(addr_east6) & !is.na(addr_east5), .(sum(addr_east5 == addr_east_recent, na.rm = T), .N)]
data_baseline[is.na(addr_north6) & !is.na(addr_north5), .(sum(addr_north5 == addr_north_recent, na.rm = T), .N)]


# imaging data




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
