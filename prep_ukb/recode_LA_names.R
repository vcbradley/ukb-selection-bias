library(data.table)


las = fread(file.choose())


las_melted = melt(las, id.vars = 'num')
las_melted = las_melted[value != '']
las_melted[, variable := NULL]
setnames(las_melted, old = names(las_melted), new = c('LA_num', 'LA_name'))

las_melted[grepl('london', tolower(LA_name))]

las_melted[order(LA_name)]

las_melted[grepl('north', tolower(LA_name))]

las_melted[LA_num == 48]


lasv2 = fread(file.choose())
setnames(lasv2, c('la_num', 'la_name'))

geos = fread(file.choose())


geos

geos[county != '']
geos[grepl('UA', county) & LA == '', LA := 'UA']
geos[, county := gsub(' UA', '', county)]
geos[county != '']
geos[, la_merged := ifelse(LA == 'UA', county, LA)]

geos


temp = merge(geos[LA != ''], las_melted, by.x = 'la_merged', by.y = 'LA_name', all = T)

temp[is.na(ID), ]
temp[is.na(LA_num), ]



temp2 = merge(geos[LA != ''], lasv2, by.x = 'la_merged', by.y = 'la_name', all = T)

# merge using both 'and' separated and not 'and' separated versions
temp3 = merge(geos[LA != ''], rbind(lasv2, las_melted, use.names = F), by.x = 'la_merged', by.y = 'la_name', all = T)
temp3

temp3_matched = unique(temp3[!is.na(ID) & !is.na(la_num)])
temp3_matched[, length(unique(la_num))]

temp3[is.na(ID) & !(la_num %in% temp3_matched$la_num), ][order(la_num)]

temp3[is.na(la_num)]

temp3_matched

#hardcode some matches based on googling 
hardcoded = data.table(la_num = c(4, 6, 7, 12, 21, 25, 48, 49, 50, 51, 52, 52, 53, 54, 54, 55, 55, 56, 57, 59, 60, 61, 129, 137)
                       , ID = c('E06000047', 'E06000047', 'E06000047', 'E06000010', 'E06000019', 'E06000023'
                                , 'E06000048', 'E06000048', 'E06000050', 'E06000050', 'E06000049', 'E06000049'
                                , 'E06000049', 'E06000051', 'E06000051', 'E06000051', 'E06000051', 'E06000052'
                                , 'E06000052', 'E06000054', 'E06000054', 'E06000054', 'E07000132', 'E07000146'
                       ))
hardcoded

# rbind our matched and hardcoded together
matched = rbind(merge(hardcoded, lasv2, all.x = T), temp3_matched[, .(la_num, ID, la_merged)], use.names = F)

#merge in geo info
matched = merge(matched, geos, all.y = T)

write.csv(matched, file = '~/Documents/Oxford/mini-project-1/data/_data/MATCHED_census_LAs.csv', row.names = F)
