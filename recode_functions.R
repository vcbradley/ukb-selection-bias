#recode_functions.R


replaceNA = function(DT, col.names) {
  for (i in col.names)
    DT[is.na(get(i)), (i):=0]
}

setNumeric = function(DT, col.names){
	for (i in col.names)
		DT[, (i) := as.numeric(get(i))]
}


#r = vector of employment status indicator data
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