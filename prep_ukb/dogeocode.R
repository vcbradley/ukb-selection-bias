## data recode
rm(list = ls())
library('data.table')
library('memisc')
library('dplyr')
library('stringr')
library('MESS')
setwd('/well/nichols/users/bwj567/data')

#source code with geo transform functions
source('/well/nichols/users/bwj567/mini-project-1/prep_ukb/geo_transforms.R')


# source data for postcode <-> easting/northing
# https://www.getthedata.com/open-postcode-geo-api

# awk code to just get 'live' postcodes
# awk -F "," '{ if($2 == "live") { print }}' open_postcode_geo.csv > open_postcode_geo_live.csv


PC_to_OSGB = fread('open_postcode_geo/open_postcode_geo_live.csv')
setnames(PC_to_OSGB
	, old = names(PC_to_OSGB)
	, new = c('postcode', 'status', 'type', 'easting', 'northing', 'quality', 'country', 'latitude', 'longitude', 'postcode_no_space', 'pcd7', 'pcd8', 'postcode_area', 'postcode_district', 'postcode_sector', 'outcode', 'incode'))
PC_to_OSGB


PC_to_LA = fread('postcode_to_LA.csv')

postcodes = merge(PC_to_LA, PC_to_OSGB)


postcodes[, easting_agg := substr(easting, 1, 3)]
postcodes[, northing_agg := substr(northing, 1, 3)]


LAs = postcodes[, .(N_LAs = length(unique(lad11nm)), LA = max(lad11nm)), .(northing_agg, easting_agg)]

split_LAs = merge(postcodes, LAs[N_LAs > 1,], by = c('northing_agg', 'easting_agg'))[, .N, .(northing_agg, easting_agg, lad11nm)]
split_LAs = merge(split_LAs, split_LAs[, .(max_n = max(N)), .(northing_agg, easting_agg)])
split_LAs[, is_max := ifelse(N == max_n, 1, 0)]
split_LAs[, is_tie := ifelse(sum(is_max) > 1 & is_max == 1, 1, 0), .(northing_agg, easting_agg)]
split_LAs[is_tie == 1, tiebreak := runif(sum(is_tie))]
split_LAs[is_tie == 1, max_tiebreak := max(tiebreak), .(northing_agg, easting_agg)]
split_LAs[, win_tiebreak := ifelse(max_tiebreak == tiebreak, 1 , 0)]

split_LAs[(is_max == 1 & is_tie == 0) | (is_tie == 1 & win_tiebreak == 1), .N]
split_LAs[, .(.N), .(northing_agg, easting_agg)]


OSGB_to_LAs = rbind(LAs[N_LAs == 1, .(northing_agg, easting_agg, LA)], split_LAs[(is_max == 1 & is_tie == 0) | (is_tie == 1 & win_tiebreak == 1), .(northing_agg, easting_agg, lad11nm)], use.names = F)

# check for 1:1 pairings
OSGB_to_LAs[, .N, .(northing_agg, easting_agg, LA)][order(N, decreasing = T)]

# check that we have all northing/easting combos
nrow(postcodes[, .N, .(northing_agg, easting_agg)])
nrow(OSGB_to_LAs)

OSGB_to_LAs[, easting_num := as.numeric(easting_agg) * 1000]
OSGB_to_LAs[, northing_num := as.numeric(northing_agg) * 1000]




# calculate lat and lon for each aggregate easting/northing
latlon = data.table(t(apply(OSGB_to_LAs[,.(easting_num, northing_num)], 1, function(x){
	unlist(OSGtoLatLon(easting = x[1], northing = x[2], newdatum = 'WGS84'))
	})))
geocodes = cbind(OSGB_to_LAs, latlon)


# write out to CSV
write.csv(geocodes, file = 'geocodes.csv', row.names = F)






