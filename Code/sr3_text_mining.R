## -----------------------------------------------------------------------------
##
## Project: Summer Research Project
##
## Script purpose: Text Mining of hd 
##
## Date: 04/06/2024
## Author: BOD
##
## -----------------------------------------------------------------------------



##-----------------------------------------------------------------------------
# Text Mining: Property Type
##-----------------------------------------------------------------------------

hd$Remove <- F # column to identify data rows to remove
hd$MultiUnitProperty <- F # to identify properties with multi-units (retail + accom), will be removed

# update the property category to broad {house, land, apt}
# keep original property types 
hd$PropertyCategory_Org <- hd$PropertyTypeOriginal
hd <- dplyr::select(hd, -PropertyTypeOriginal)
hd$PropertyType_Org <- hd$PropertyType 



##------------------------- Inquire Subset -------------------------

# Inquire the following property categories 
land_types <- c('0.16 Acre Site', 'Agricultural Land', 'Development Land', 'Farm', 'Farm Land', 'Site')
industrial_types <- c('Businesses', 'Commercial Site', 'Industrial Site',
                      'Industrial Unit', 'Industrial Units', 'Live-Work Unit', 
                      'Office Space', 'Pub & Resturants', 'Pubs & Restaurants', 'Retail Units', 'Serviced Office', 
                      'Restaurant / Bar / Hotel', 'Retail Unit')

# combine and inquire about all 
inquire_subset <- hd[hd$PropertyType %in% c(land_types, industrial_types), ]


#### Planning Permission

# text mine for planning permission - check there are no houses 
planning_ptn <- ('planning\\s*(-)?\\s*permission')
planning_words_inx <- grepl(planning_ptn, inquire_subset$Description, ignore.case = T, perl = T)

# first remove the planning permission 
hd$Remove[(hd$Id %in% inquire_subset$Id[planning_words_inx])] <- T

# keep some with planning permission (extensions not houses)
hd$Remove[hd$Id %in% c(615654, 555174)] <- F


#### Apartments and Houses

# text mining pattern for apartments / houses
house_words_ptn <- ('house|detached|semi\\s*(-)?\\s*detached|terraced|end\\s*(-)?\\s*of\\s*(-)?\\s*terrace|bungalow|cottage')
apt_words_ptn <- ('apartment|studio|flat|penthouse|duplex')

# matches for apartments and hosues 
house_words_inx <- grepl(house_words_ptn, inquire_subset$Description, ignore.case = T, perl =T)
apt_words_inx <- grepl(apt_words_ptn, inquire_subset$Description, ignore.case = T, perl =T)

# review all, remoove all 
hd$Remove[hd$Id %in% inquire_subset$Id] <- T

# 3 detached ID identified 
hd$PropertyType[hd$Id %in% c(532925, 516664, 511955)] <- 'Detached'
hd$Remove[hd$Id %in% c(532925, 516664, 511955)] <- F

# 2 terraced ID identified
hd$PropertyType[hd$Id %in% c(527762, 532145)] <- 'Townhouse'
hd$Remove[hd$Id %in% c(527762, 532145)] <- F



##------------------------- Investment Properties -------------------------

# Review investment properties 
investment_inquire_subset <- hd[hd$PropertyType %in% c('Investment', 'Investment Property'),]

#### Apartments and Houses
# text mining pattern for apartments / houses
house_words_inx <- grepl(house_words_ptn, investment_inquire_subset$Description, ignore.case = T, perl =T)
apt_words_inx <- grepl(apt_words_ptn, investment_inquire_subset$Description, ignore.case = T, perl =T)


# mainly consist of multi apartment blocks, retail units + accommodation, remove all
hd$Remove[hd$Id %in% investment_inquire_subset$Id] <- T


# Remove the flagged rows 
hd <- hd[!hd$Remove, ]




##------------------------- Rectify Property Categories -------------------------

# 1 land property is detached, rectify a land category. 
hd$PropertyCategory <- hd$PropertyCategory_Org
hd$PropertyType[hd$PropertyCategory_Org == 'Land'] <- 'Detached'
hd$PropertyCategory[hd$PropertyCategory_Org=='Land'] <- 'House'
# 21000 missing categories


#### Group the property types 
# Houses 
house_types <- c('Bungalow', 'Country House', 'Detached House, Terraced House', 
                 'Semi-Detached House, Terraced House', 'Semi-Detached House, Townhouse',
                 'Houses', 'Dormer', 'Detached House, End of Terrace House, Semi-Detached House',
                 'Bungalow, Semi-Detached House, Terraced House', 'Cottage', 'Dormer, Semi-Detached House',
                 'House, Semi-Detached House, Terraced House', 'Period House', 'Detached House, Semi-Detached House')
hd$PropertyType[hd$PropertyType %in% house_types] <- 'House'

hd$PropertyType[hd$PropertyType %in% c('End Of Terrace House', 'End of Terrace House','End Of Terrace', 'End of Terrace')] <- 'End-of-Terrace'
hd$PropertyType[hd$PropertyType %in% c('Detached', 'Detached House')] <- 'Detached'
hd$PropertyType[hd$PropertyType %in% c('Semi-D', 'Semi-Detached', 'Semi-Detached House', 'House, Semi-Detached House')] <- 'Semi-Detached'
hd$PropertyType[hd$PropertyType %in% c('Terrace', 'Terraced House', 'House, Terraced House')] <- 'Terraced'

# Apartments 
apt_types <- c('Apartment', 'Apartment, Penthouse', 'Studio', 'Penthouse', 'Studio Apartment', 'Flat')
hd$PropertyType[hd$PropertyType %in% apt_types] <- 'Apartment'

# Ambigious 
apt_house_types <- c('Apartment, Detached House, Semi-Detached House', 'Apartment, Townhouse')
hd$PropertyType[hd$PropertyType %in% apt_house_types] <- 'Apartment_House'


#### New Developments
#- no information on what specific property it is, these will be investigated but first categorised 
new_development_addresses_ptn <- ('Trimblestown, Goatstown Road|The Alder  Trimbleston|Mariners Avenue, Mariners Way|The Mill Tree, Ratoath|The Green, Ballinageragh|Dromdiah Park|Cois Teampall|Sandymount Castle Court|Barn Elms|Woodbrook Park|Woodbrook Avenue|Woodbrook Way|Oldtown Woods|Woodbrook Park|Silverbrook Park|The Mil Tree, Ratoath |Reldare, Model Farm Road|Chapel Hill, Chapel Lane, Garristown|Palace Fields, Tuam' )
new_developemnt_inx <- grepl(new_development_addresses_ptn, hd$RawAddress, ignore.case = T, perl =T)
hd$PropertyType[new_developemnt_inx] <- 'New Development'




##------------------------- Duplex -------------------------

# Duplex Properties 
duplex_pttn <- ('duplex')
# matches of Duplex properties 
duplex_words_inx <- grepl(duplex_pttn, hd$Description, ignore.case = T, perl =T)


# for the detached (duplex) 
hd$PropertyType[hd$PropertyType=='Detached' & duplex_words_inx] <- 'Duplex' # actually duplex
hd$PropertyType[hd$Id == 517146] <- 'Apartment' # apartment identified 
hd$PropertyType[hd$Id %in% c(596069, 589222)] <- 'Detached' # actually detached 

# Detached House, House, Terraced House 
hd$PropertyType[hd$PropertyType == 'Detached House, House, Terraced House' & duplex_words_inx] <- 'Semi-Detached' 

# End of Terrace
hd$PropertyType[hd$PropertyType == 'End-of-Terrace' & duplex_words_inx] <- 'Duplex'
hd$PropertyType[hd$Id == 625525] <- 'End-of-Terrace'

# House
hd$PropertyType[hd$PropertyType == 'House' & duplex_words_inx] <- 'Terraced'

# Semi Detached
#View(hd[hd$PropertyType == 'Semi-Detached' & duplex_words_inx, ]) 
hd$PropertyType[hd$PropertyType == 'Semi-Detached' & duplex_words_inx] <- 'Duplex' 
hd$PropertyType[hd$Id == 537297] <- 'Apartment'

# Terraced
#View(hd[hd$PropertyType == 'Terraced' & duplex_words_inx, ]) 
hd$PropertyType[hd$PropertyType == 'Terraced' & duplex_words_inx] <- 'Duplex' 
hd$PropertyType[hd$Id %in% c(565677, 576238)] <- 'Apartment'
hd$MultiUnitProperty[hd$Id %in% c(565677, 576238)] <- T

# Townhouse
hd$PropertyType[hd$PropertyType == 'Townhouse' & duplex_words_inx] <- 'Duplex' 

# Apartment
hd$PropertyType[hd$PropertyType == 'Apartment' & duplex_words_inx] <- 'Duplex' 

# the Detached House, Duplex are a new developments (mainly 3,2 semiD)
hd$PropertyType[hd$PropertyType== 'Detached House, Duplex'] <- 'Semi-Detached'





##------------------------- End of Terrace -------------------------

# End of terrace Properties 
eot_pttn <- 'end\\s*(-)?\\s*of\\s*(-)?\\s*terrace'
# matches for end of terrace
eot_words_inx <- grepl(eot_pttn, hd$Description, ignore.case = T, perl =T)


# for the apartment
#View(hd[hd$PropertyType == 'Apartment' & eot_words_inx,]) # all apartments 

# Detached
#View(hd[hd$PropertyType == 'Detached' & eot_words_inx,]) # all end of terrace bar 1 
hd$PropertyType[hd$PropertyType == 'Detached' & eot_words_inx] <- 'End-of-Terrace'
hd$PropertyType[hd$Id == 623327] <- 'Detached'

# House 
#View(hd[hd$PropertyType == 'House' & eot_words_inx,]) # all eot 
hd$PropertyType[hd$PropertyType == 'House' & eot_words_inx] <- 'End-of-Terrace'

# Semi-Detached 
#View(hd[hd$PropertyType == 'Semi-Detached' & eot_words_inx,]) # all eot 
hd$PropertyType[hd$PropertyType == 'Semi-Detached' & eot_words_inx] <- 'End-of-Terrace'

# Terraced
#View(hd[hd$PropertyType == 'Terraced' & eot_words_inx,]) # all eot 
hd$PropertyType[hd$PropertyType == 'Terraced' & eot_words_inx] <- 'End-of-Terrace'
hd$PropertyType[hd$Id %in% c(552996, 540862, 566983)] <- 'Terraced' # some terraced 

# Townhouse
#View(hd[hd$PropertyType == 'Townhouse' & eot_words_inx,]) # all eot 
hd$PropertyType[hd$PropertyType == 'Townhouse' & eot_words_inx] <- 'End-of-Terrace'





##------------------------- Semi Detached -------------------------

# semi detached pattern
semid_pttn <- 'semi\\s*(-)?\\s*detached'
# matches for semi detached properties 
semid_words_inx <- grepl(semid_pttn, hd$Description, ignore.case = T, perl =T)

# Apartment 
#View(hd[hd$PropertyType == 'Apartment' & semid_words_inx,]) # all apartments bar some 
hd$PropertyType[hd$Id %in% c(539996, 552702, 527040, 554995, 534326)] <- 'Semi-Detached'

# Detached 
#View(hd[hd$PropertyType == 'Detached' & semid_words_inx,]) # all apartments bar some 
hd$PropertyType[hd$PropertyType == 'Detached' & semid_words_inx] <- 'Semi-Detached'
hd$PropertyType[hd$Id %in% c(562710, 595511, 564086, 552428, 508917, 514157, 562533, 
                             573404, 508284, 555263, 540267, 505759, 559157, 589934,
                             552507, 564564, 611414, 558962, 555633, 585314, 560399)] <- 'Detached'
hd$PropertyType[hd$Id == 550364] <- 'Apartment'

# Duplex 
#View(hd[hd$PropertyType == 'Duplex' & semid_words_inx,]) # all apartments bar some 
hd$PropertyType[hd$Id == 528029] <- 'Apartment'

# House 
hd$PropertyType[hd$PropertyType == 'House' & semid_words_inx] <- 'Semi-Detached'

# Terraced
# all semi-d apart from a few  
hd$PropertyType[hd$PropertyType == 'Terraced' & semid_words_inx] <- 'Semi-Detached'
hd$PropertyType[hd$Id %in% c(594900, 586920, 539908, 544692, 588042)] <- 'Terraced'
hd$PropertyType[hd$Id == 562032] <- 'End-of-Terrace'

# Townhouse 
#View(hd[hd$PropertyType == 'Townhouse' & semid_words_inx,]) # all semi-d
hd$PropertyType[hd$PropertyType == 'Townhouse' & semid_words_inx] <- 'Semi-Detached'

hd <- hd[hd$Id != 515808,] # site with planning permission



##------------------------- Mews -------------------------
hd$PropertyType[hd$PropertyType == 'Mews'] <- 'Terraced' # all terraced 




##------------------------- Apartment -------------------------

# text pattern for apartemtns 
apt_pttn <- 'apartment|flat|penthouse|duplex'
# matches of apartments 
apt_words_inx <- grepl(apt_pttn, hd$Description, ignore.case = T, perl =T)


# Apartment_House 
hd$PropertyType[hd$PropertyType == 'Apartment_House'] <- 'Apartment'

# Detached 
#View(hd[hd$PropertyType == 'Detached',]) #all detached

# End of Terrace 
#View(hd[hd$PropertyType == 'End of Terrace',]) # some apt
hd$PropertyType[hd$Id %in% c(538419, 603033)] <- 'Apartment'

# House
#View(hd[hd$PropertyType == 'House' & apt_words_inx, ]) # some apt
hd$PropertyType[hd$Id %in% c(566168, 507883, 566168)] <- 'Apartment'

# Semi-Detached 
#View(hd[hd$PropertyType == 'Semi-Detached' & apt_words_inx, ])
hd$PropertyType[hd$Id %in% c(611301, 601820, 589935, 588784, 569904, 538264)] <- 'Apartment' # some apt 
hd$MultiUnitProperty[hd$Id == 588784] <- T

# Terraced 
#View(hd[hd$PropertyType == 'Terraced' & apt_words_inx, ])
hd$PropertyType[hd$Id %in% c(513435, 542945, 565392,  594334)] <- 'Apartment'
hd$MultiUnitProperty[hd$Id %in% c( 542945, 565392, 594334 )] <- T


# Townhouse 
#View(hd[hd$PropertyType == 'Townhouse' & apt_words_inx, ])
hd$PropertyType[hd$Id %in% c(506684, 625263, 561868)] <- 'Apartment'

# Apartment, Duplex
hd$PropertyType[hd$PropertyType=='Apartment, Duplex'] <- 'New Development'

# Apartment, Duplex, Semi-Detached House
hd$PropertyType[hd$PropertyType == 'Apartment, Duplex, Semi-Detached House'] <- 'Apartment'




##------------------------- Detached -------------------------

# detached pattern
detached_pttn <- '^(?!.*semi(-)?).*detached'

# detached match
detached_words_inx <- grepl(detached_pttn, hd$Description, ignore.case = T, perl =T)

# Semi - Detached 
#View(hd[hd$PropertyType == 'Semi-Detached' & detached_words_inx, ]) # all detached bar some 
hd$PropertyType[hd$PropertyType == 'Semi-Detached' & detached_words_inx] <- 'Detached'
hd$PropertyType[hd$Id %in% c(593924, 574158, 505637, 556536, 551937, 559956, 593924)] <- 'Semi-Detached'

# Apartment 
#View(hd[hd$PropertyType == 'Apartment' & detached_words_inx, ]) # all apt

# End of Terrace 
#View(hd[hd$PropertyType == 'End of Terrace' & detached_words_inx, ]) # all eot bar 1
hd$PropertyType[hd$Id == 624740] <- 'Detached'

# Terraced
#View(hd[hd$PropertyType == 'Terraced' & detached_words_inx, ]) # all terr bar some 
hd$PropertyType[hd$Id %in% c(594409, 599138)] <- 'Detached'


# Townhouse
#View(hd[hd$PropertyType == 'Townhouse' & detached_words_inx, ]) # most 'detached' and 'townhouse'
hd$PropertyType[hd$Id %in% c(596194)] <- 'Detached'

# House
#View(hd[hd$PropertyType == 'House' & detached_words_inx, ]) # all detached
hd$PropertyType[hd$PropertyType == 'House' & detached_words_inx ] <- 'Detached'





##------------------------- Terraced -------------------------

# terraced pattern
terraced_pttn <- 'mid\\s*(-)?\\s*terrace'

# matches for terrace
terraced_words_inx <- grepl(terraced_pttn, hd$Description, ignore.case = T, perl =T)

# set all mid-terrace to terrace houses 
hd$PropertyType[terraced_words_inx] <- 'Terraced'

terraced_pttn <- '^(?!.*end).*terrace'
terraced_words_inx <- grepl(terraced_pttn, hd$Description, ignore.case = T, perl =T)


# Semi - Detached 
#View(hd[hd$PropertyType == 'Semi-Detached' & terraced_words_inx, ]) # all semi bar some 
hd$PropertyType[hd$Id %in% c(537306, 533468, 530096, 525518, 512700, 512190, 552084, 548895, 553895, 576058, 584623, 600678)] <- 'Terraced'

# House
hd$PropertyType[hd$PropertyType == 'House' & terraced_words_inx] <- 'Terraced'

# New Developmetn 
hd$PropertyType[hd$PropertyType == 'New Development' & terraced_words_inx] <- 'Terraced'




##------------------------- Townhouse -------------------------

# pattern for townhouse
townhouse_pttn <- 'townhouse'

# matches for townhouse
townhouse_words_inx <- grepl(terraced_pttn, hd$Description, ignore.case = T, perl =T)

hd$PropertyType[hd$PropertyType == 'Terraced' & townhouse_words_inx] <- 'Townhouse'



##------------------------- NA Property Type -------------------------

# extract NA types 
na_type <- hd[is.na(hd$PropertyType), ]

# 55 without description can be removed
hd$Remove[is.na(hd$Description) & is.na(hd$PropertyType)] <- T
hd <- hd[!hd$Remove, ] #drop removed values 


#### mine for key words 
eot_words_inx <- grepl(eot_pttn, na_type$Description, ignore.case = T, perl =T)
duplex_words_inx <- grepl(duplex_pttn, na_type$Description, ignore.case = T, perl =T)
apt_words_inx <- grepl(apt_pttn, na_type$Description, ignore.case = T, perl =T)
semid_words_inx <- grepl(semid_pttn, na_type$Description, ignore.case = T, perl =T)
detached_words_inx <- grepl(detached_pttn, na_type$Description, ignore.case = T, perl =T)
terraced_words_inx <- grepl(terraced_pttn, na_type$Description, ignore.case = T, perl =T)
townhouse_words_inx <- grepl(townhouse_pttn, na_type$Description, ignore.case = T, perl =T)

# assign property type as the key word match 
na_type$PropertyType[eot_words_inx] <- 'End-of-Terrace'
na_type$PropertyType[duplex_words_inx] <- 'Duplex'
na_type$PropertyType[apt_words_inx] <- 'Apartment'
na_type$PropertyType[detached_words_inx] <- 'Detached'
na_type$PropertyType[semid_words_inx] <- 'Semi-Detached'
na_type$PropertyType[terraced_words_inx] <- 'Terraced'
na_type$PropertyType[townhouse_words_inx] <- 'Townhouse'

#View(na_type[is.na(na_type$PropertyType), ]) # mainly new development 

# specific ID to fix 
na_type$PropertyType[na_type$Id == 580357] <- 'Semi-Detached'
na_type$PropertyType[na_type$Id %in% c(544406, 544540, 558940, 559187, 562871, 595567, 624924, 569790, 574105, 583312, 554244, 581656)] <- 'Semi-Detached'
na_type$PropertyType[is.na(na_type$PropertyType)] <- 'New Development'


# move the results to hd 
hd$PropertyType[hd$Id %in% na_type$Id] <- na_type$PropertyType[hd$Id %in% na_type$Id]

# delete unnecessary (sites)
hd$Remove[hd$Id %in% c(604728, 614487)] <- T
hd <- hd[!hd$Remove, ] # drop removed values 




##------------------------- New Development -------------------------

#View(hd[hd$PropertyType == 'New Development', ])
colSums(is.na(hd[hd$PropertyType == 'New Development', ]))

# create a subset 
new_dev <- hd[hd$PropertyType == 'New Development', ]

#### check property types 
eot_words_inx <- grepl(eot_pttn, new_dev$Description, ignore.case = T, perl =T)
duplex_words_inx <- grepl(duplex_pttn, new_dev$Description, ignore.case = T, perl =T)
apt_words_inx <- grepl(apt_pttn, new_dev$Description, ignore.case = T, perl =T)
semid_words_inx <- grepl(semid_pttn, new_dev$Description, ignore.case = T, perl =T)
detached_words_inx <- grepl(detached_pttn, new_dev$Description, ignore.case = T, perl =T)
terraced_words_inx <- grepl(terraced_pttn, new_dev$Description, ignore.case = T, perl =T)
townhouse_words_inx <- grepl(townhouse_pttn, new_dev$Description, ignore.case = T, perl =T)

# get row sums to see hwo many proeprty types are mentioned for each description 
df <- data.frame(Id = new_dev$Id, eot_words_inx, duplex_words_inx, apt_words_inx, semid_words_inx, detached_words_inx, terraced_words_inx, townhouse_words_inx) #TF for each index
df <- data.frame(df, sum = rowSums(df[-1])) # row sums to see how many property types per ID 
df <- df[!is.na(df$Id),]

# remove descriptions that do not mention any types {THESE COULD BE SOURCED ONLINE}
hd$Remove[hd$Id %in% df$Id[df$sum == 0]] <- T; hd<- hd[!hd$Remove, ]

# investigate the multi entry ones 
#View(new_dev[new_dev$Id %in% df$Id[df$sum > 1], ]) 

# rectify Ryno Developments 
hd$PropertyType[grepl('Ryno Developments', hd$Description)] <- 'Semi-Detached'
hd$Beds[grepl('Ryno Developments', hd$Description)] <- 4

# rectify Camberly Mews
hd$PropertyType[grepl('Camberley Mews', hd$Description)] <- 'Detached'
hd$Beds[grepl('Camberley Mews', hd$Description)] <- 4

# rectify The Drive in Janeville
hd$PropertyType[grepl('The Drive in Janeville', hd$Description)] <- 'Detached'
hd$Beds[grepl('The Drive in Janeville', hd$Description)] <- 4

# no info for Station Road, Newbridge, Co. Kildare.
hd$Remove[grepl('Station Road, Newbridge, Co. Kildare.', hd$Description)] <- T; hd<- hd[!hd$Remove, ]

# rectify Skylark detached houses
hd$PropertyType[grepl('Skylark detached house', hd$Description)] <- 'Detached'
hd$Beds[grepl('Skylark detached house', hd$Description)] <- 4

# remaining properties 
hd$PropertyType[hd$Id == 621529] <- 'Duplex'
hd$PropertyType[hd$Id == 605325] <- 'Detached'; hd$Beds[hd$Id == 605325] <- 3
hd$PropertyType[hd$Id == 531897] <- 'End-of-Terrace'; hd$Beds[hd$Id == 605325] <- 4


# for correct 1:1 matches - assign property type 
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$eot_words_inx]] <- 'End-of-Terrace'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$duplex_words_inx]] <- 'Duplex'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$apt_words_inx]] <- 'Apartment'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$semid_words_inx]] <- 'Semi-Detached'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$detached_words_inx]] <- 'Detached'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$terraced_words_inx]] <- 'Terraced'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$townhouse_words_inx]] <- 'Townhouse'

# rectiffy the final few entries 
#View(hd[hd$PropertyType == 'New Development', ])
hd$PropertyType[hd$Id == 533900] <- 'Apartment'
hd$PropertyType[hd$Id == 580550] <- 'Semi-Detached'
hd$PropertyType[hd$Id == 537721] <- 'Detached'
hd$PropertyType[hd$Id == 625986] <- 'Detached'
hd$PropertyType[hd$Id == 528151] <- 'Semi-Detached'

# remove the remainder (unknown)
hd <- hd[hd$PropertyType != 'New Development', ]






##------------------------- House -------------------------


# some NA descriptions 
# remove now {COULD BE IMPUTED}
hd$Remove[hd$PropertyType == 'House' & is.na(hd$Description)] <- T; hd <- hd[!hd$Remove, ]

# create a subset
houses <- hd %>% filter(PropertyType == 'House')


#### check property types 
eot_words_inx <- grepl(eot_pttn, houses$Description, ignore.case = T, perl =T)
duplex_words_inx <- grepl(duplex_pttn, houses$Description, ignore.case = T, perl =T)
apt_words_inx <- grepl(apt_pttn, houses$Description, ignore.case = T, perl =T)
semid_words_inx <- grepl(semid_pttn, houses$Description, ignore.case = T, perl =T)
detached_words_inx <- grepl(detached_pttn, houses$Description, ignore.case = T, perl =T)
terraced_words_inx <- grepl(terraced_pttn, houses$Description, ignore.case = T, perl =T)
townhouse_words_inx <- grepl(townhouse_pttn, houses$Description, ignore.case = T, perl =T)

# get row sums to see hwo many proeprty types are mentioned for each description 
df <- data.frame(Id = houses$Id, eot_words_inx, duplex_words_inx, apt_words_inx, semid_words_inx, detached_words_inx, terraced_words_inx, townhouse_words_inx)
df <- data.frame(df, sum = rowSums(df[-1]))
df <- df[!is.na(df$Id),]

# for correct matches - assign property type 
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$eot_words_inx]] <- 'End-of-Terrace'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$duplex_words_inx]] <- 'Duplex'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$apt_words_inx]] <- 'Apartment'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$semid_words_inx]] <- 'Semi-Detached'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$detached_words_inx]] <- 'Detached'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$terraced_words_inx]] <- 'Terraced'
hd$PropertyType[hd$Id %in% df$Id[df$sum==1 & df$townhouse_words_inx]] <- 'Townhouse'

# assume bunglows or cottages are detached 
hd$PropertyType[hd$Id %in% houses$Id & grepl('bungalow', hd$Description, perl = T, ignore.case = T)] <- 'Detached'
hd$PropertyType[hd$Id %in% houses$Id & grepl('cottage', hd$Description, perl = T, ignore.case = T)] <- 'Detached'

# update houses 
houses <- hd %>% filter(PropertyType == 'House')

# detached houses are there (no semi's) so we can searhc for detahced 
detached_pttn <- 'detached'
detached_words_inx <- grepl(detached_pttn, houses$Description, ignore.case = T, perl =T)
hd$PropertyType[hd$Id %in% houses$Id[detached_words_inx]] <- 'Detached'

# update houses 
houses <- hd %>% filter(PropertyType == 'House')

# some end of terrace
hd$PropertyType[hd$Id %in% c(548306, 552170, 615227, 588790, 585108, 552170, 548306)] <- 'End-of-Terrace'

# the rest are terraced (mentioning terrace)
terraced_words_inx <- grepl('terrace', houses$Description, ignore.case = T, perl =T)
hd$PropertyType[hd$Id %in% houses$Id[terraced_words_inx]] <- 'Terraced'

# update houses 
houses <- hd %>% filter(PropertyType == 'House')

# mainly city numbered houses - assuming semi-d
# some detached 
hd$PropertyType[hd$Id %in% c(507572, 526489, 532980, 536532, 543229, 544347, 558752,
                             555517, 555720, 562617, 566144, 572805, 574746, 574849,
                             585134, 591658, 603620, 612038, 613488)] <- 'Detached'
houses <- hd %>% filter(PropertyType == 'House')

hd$PropertyType[hd$Id %in% houses$Id] <- 'Semi-Detached'


#### Update the Property Catgegories 
hd$PropertyCategory <- ifelse(hd$PropertyType %in% c('Apartment', 'Duplex'), 'Apartment', 'House')

#### Remove Values for multiunit properties and the column
hd <- hd[!hd$MultiUnitProperty, ]
hd <- dplyr::select(hd, -c('MultiUnitProperty'))

#### Remove the PropertyCategory_Org column 
hd <- dplyr::select(hd, -c('PropertyType_Org', 'PropertyCategory_Org'))

#### ********* ********* ********* CHECKPOINT ********* ********* ********* ####

# this data contains property types corrected 
write.csv(hd, './Data/hd_mined_p_types.csv', row.names = F) 
hd <- readr::read_csv(here("Data/hd_mined_p_types.csv"))





##-----------------------------------------------------------------------------
# Text Mining: Bedrooms and Bathrooms
##-----------------------------------------------------------------------------


# remove values with no description and either no beds or baths  
hd$Remove[is.na(hd$Description) & (is.na(hd$Beds) | is.na(hd$Baths))] <- T
hd <- hd[!hd$Remove, ]

## subset missing bedrooms and bathrooms 
no_bed_bath <- hd[is.na(hd$Beds) & is.na(hd$Baths), ]


# some developments, remove 
no_bed_bath$Remove[grepl('2, 3 & 4 ', no_bed_bath$Description)] <- T

# remove values that do not mention bed 
no_bed_bath$Remove[grepl('bed', no_bed_bath$Description)] <- T

# many advertise developemtns with no info on proeprty 
no_bed_bath$Remove[grepl('development', no_bed_bath$Description)] <- T

# remove all the rest 
hd$Remove[hd$Id %in% no_bed_bath$Id] <- T
hd$Remove[hd$Id %in% c(547027, 511342)] <- F
hd <- hd[!hd$Remove, ]



#### Missing Bedrooms 

# number of missing bedrooms 
no_beds <- hd[is.na(hd$Beds), ]

# new developments can be removed 
no_beds$Remove[grepl('NEWTOWN MANOR, CASTLETROY|Sli Na Manach,', no_beds$Description)] <- T

# more developments and multiunit peroperty identified 
no_beds$Remove[no_beds$Id %in% c(531101, 560416, 625726, 626052)] <- T

# Update hd and remove from no_beds
hd$Remove[hd$Id %in% no_beds$Id[no_beds$Remove]] <- T
no_beds <- no_beds[!no_beds$Remove, ]

# Regular expression to match the number or word before 'bed'
bed_pttn <- "(\\b\\w+\\b)\\s+bed"

# indeces for number of beds 
bed_match <- str_match(no_beds$Description, bed_pttn)[,2]

bed_match_num <- as.numeric(bed_match) # convert string to number 
bed_match_val <-as.numeric(recode(bed_match, 'one'=1, 'two'=2, 'three'=3, 'four'=4)) # change text to number
bed_match_val[is.na(bed_match_val)] <- bed_match_num[is.na(bed_match_val)] # combine both 


# input remainder 
no_beds$Beds <- bed_match_val
no_beds$Beds[no_beds$Id %in% c(562342, 547027, 511342)] <- 3
no_beds$Beds[no_beds$Id == 581344] <- 4

# update hd 
hd$Beds[hd$Id %in% no_beds$Id] <- no_beds$Beds

# remove the remainder (unkown)
hd$Remove[is.na(hd$Beds)] <- T;  hd <- hd[!hd$Remove, ]



#### Missing Bathrooms

# number of missing bathrooms 
no_baths <- hd[is.na(hd$Baths), ]

# many developments to remove 
no_baths$Remove[grepl('development', no_baths$Description)] <- T

# phrases or number before. bath 
# Regular expression to match the number or word before 'bath'
bath_pttn <- "(\\b\\w+\\b)\\s+bath"

# indeces for number of bathrooms 
bath_match <- str_match(no_baths$Description, bath_pttn)[,2]

# convert string to number 
bath_match_num <- as.numeric(bath_match) 
# very few matches here, 

# count occuraces of key words 
bath_pttn <- "\\b(bathroom|ensuite|Bathroom|en-suite|toilet|Toilet)\\b"

# Use str_count to count the occurrences of the words in each string
occurrences <- str_count(no_baths$Description, bath_pttn)

# rectify outliers (repitition)
occurrences[occurrences==9] <- 2
occurrences[occurrences==7] <- 3
occurrences[occurrences==6] <- 2
occurrences[!is.na(bath_match_num)] <- bath_match_num[!is.na(bath_match_num)] # updatae list with text extraction

# check the 0 values 
#no info so remove 
no_baths$Remove[occurrences==0] <- T

# update baths 
no_baths$Baths <- occurrences

# update hd 
hd$Baths[hd$Id %in% no_baths$Id] <- no_baths$Baths # the number of baths 
hd$Remove[hd$Id %in% no_baths$Id] <- no_baths$Remove # the Remove status

# remove values and column name 
hd <- hd[!hd$Remove,]
hd <- dplyr::select(hd, -c('Remove'))


#### ********* ********* ********* CHECKPOINT ********* ********* ********* ####
# this hd now has no missing values for bedrooms and bathrooms and property type 

write.csv(hd, './Data/hd_mined_p_types_bed_bath.csv', row.names = F) 
hd <- readr::read_csv(here('./Data/hd_mined_p_types_bed_bath.csv'))




##-----------------------------------------------------------------------------
# Text Mining: Property Size
##-----------------------------------------------------------------------------


# there are 3990 missing size values 

# number formatting of decimal places and thousands 
hd <- number_formatting(hd)


# fix typos in the measurement 
hd <- hd %>%
  mutate(Description = str_replace_all(Description, "4141.0m", "4.1410m")) %>%
  mutate(Description = str_replace_all(Description, "2220m", "2.220m")) %>%
  mutate(Description = str_replace_all(Description, '2068m x 1.96m', '2.068m x 1.96m')) %>%
  mutate(Description = str_replace_all(Description, "2083 x 2m", "2.083 x 2m")) %>%
  mutate(Description = str_replace_all(Description, "480m", "4.80m")) %>%
  mutate(Description = str_replace_all(Description, "176m", "1.76m"))%>%
  mutate(Description = str_replace_all(Description, "3,90m x 3.20m", "3.90m x 3.20m"))%>%
  mutate(Description = str_replace_all(Description, "105m", "1.05m"))%>%
  mutate(Description = str_replace_all(Description, "37m x 2.5m", "3.7m x 2.5m")) %>%
  mutate(Description = str_replace_all(Description, "4. 93 x 2.05m", "4.93 x 2.05m")) %>% 
  mutate(Description = str_replace_all(Description, "3 .73m", "3.73m")) %>% 
  mutate(Description = str_replace_all(Description, "1.83 x 135m", "1.83 x 1.35m")) %>%
  mutate(Description = str_replace_all(Description, "12m  x 10€", "12m  x 10m")) %>%
  mutate(Description = str_replace_all(Description, "5m  x 180", "5m  x 18m")) %>% 
  mutate(Description = str_replace_all(Description, '&nbsp;sq. m', 'sqm')) %>%
  mutate(Description = str_replace_all(Description, 'excess of 2700 sq', 'excess of 2700 sqft')) %>%
  mutate(Description = str_replace_all(Description, 'sq tt,', 'sq ft,')) %>%
  mutate(Description = str_replace_all(Description, '(1359.00ft approx) ', '1359 sqft'))%>%
  mutate(Description = str_replace_all(Description, 'approx. 1900 sq.', 'approx. 1900 sqm')) %>%
  mutate(Description = str_replace_all(Description, '11715 SQ.FT', '1,171sqft')) %>%
  mutate(Description = str_replace_all(Description, '€oe', '('))

# remove eircodes from text (due to x and numbers)
hd <- remove_eircode(hd)

# extract observations with no size 
no_size <- hd[is.na(hd$Size), ]


##-----------------------------------------------------------------------------
# Overall Property Dimensions 
##-----------------------------------------------------------------------------

# Get the overall property size 
no_size <- property_size(no_size)

# keep the metre version, fill other NA values with sqft values 
no_size$Size <- no_size$sqm
no_size$Size[is.na(no_size$sqm)] <- no_size$sqft_m[is.na(no_size$sqm)]

# remove outliers 
no_size$Size[no_size$Size < 30] <- NA #mainly one room given or garage 
no_size$Size[no_size$Size > 450] <- NA #given as sites or typos  

# fix specific entries 
no_size$Size[no_size$Id == 526328] <- sqft_to_sqm(no_size$sqm[no_size$Id == 526328]) # sqft in m value
no_size$Size[no_size$Id == 507219] <- no_size$sqft_m[no_size$Id == 507219] # sqm in sqft_m value

# drop the intermediate variables 
no_size <- dplyr::select(no_size, -c('sqm', 'sqft_m'))


##-----------------------------------------------------------------------------
# Room by Room Property Size
##-----------------------------------------------------------------------------

# update no size - extract descriptions from this 
no_size <- no_size[is.na(no_size$Size), ]

no_size <- total_area_by_room(no_size)

# at lest one is not missing 
results <- no_size[!(is.na(no_size$TotalArea_m)) | !(is.na(no_size$TotalArea_ft_m)), ]

# total area is total metres, update the rest of NA with sqft values 
results$TotalArea <- results$TotalArea_m
results$TotalArea[is.na(results$TotalArea_m)] <- results$TotalArea_ft_m[is.na(results$TotalArea_m)]

# review cases greater than 500
results$TotalArea[results$Id %in% c(595589, 578048, 559401, 551071, 550193, 537492)] <- sqft_to_sqm(results$TotalArea[results$Id %in% c(595589, 578048, 559401, 551071, 550193, 537492)])
results$TotalArea[results$Id == 588070] <- results$TotalArea[results$Id == 588070] /1000000 # given in mm 
results$TotalArea[results$TotalArea > 500] <- NA

# review cases below 20
results$TotalArea[results$Id == 588070] <- results$TotalArea_m[results$Id == 588070]/10000 # given in cm 

results$TotalArea[results$TotalArea < 20] <- NA


# update the large values 
data <- results[results$TotalArea > 500, ] 

## add results to no_size 
no_size$Size[no_size$Id %in% results$Id] <- results$TotalArea




##-----------------------------------------------------------------------------
# Update hd and investigate Size outliers 
##-----------------------------------------------------------------------------

hd$Size[hd$Id %in% no_size$Id] <- no_size$Size

#### investigate all under 40 

under40 <- hd[hd$Size < 40, ]

## apply the above methods again
under40 <- under40 %>% number_formatting() 
under40 <-  property_size(under40)

# use sqm else sqft 
under40$Size <- under40$sqm
under40$Size[is.na(under40$sqm)] <- under40$sqft_m[is.na(under40$sqm)]

# room areas 
under40 <- total_area_by_room(under40)
under40$TotalArea <- under40$TotalArea_m
under40$TotalArea[is.na(under40$TotalArea_m)] <- under40$TotalArea_ft_m[is.na(under40$TotalArea_m)]

# update the remaining size values iwth total area
under40$Size[is.na(under40$Size)] <- under40$TotalArea[is.na(under40$Size)]

# update the over 600 values as total area (if not missing)
indices <- which(under40$Size > 600 & !is.na(under40$TotalArea))
under40$Size[indices] <- under40$TotalArea[indices]

# specific entry 
under40$Size[under40$Id == 550079] <- 35.46

# assumer over 1000 values are in swft 
indices <- which(under40$Size > 1000)
under40$Size[indices] <- under40$Size[indices]/10.9743

# update the under 30 values with total area if it is larger 
indices <- which(under40$Size < 30 & (under40$TotalArea > under40$Size))
under40$Size[indices] <- under40$TotalArea[indices]

# set all under 30 as NA now 
indices <- which(under40$Size < 30)
under40$Size[indices] <- NA


### Update hd
hd$Size[hd$Id %in% under40$Id] <- under40$Size[hd$Id %in% under40$Id] 




#### Inversitgate all over 500

over500 <- hd[hd$Size > 500, ]

# apply formatting 
over500 <- over500 %>%
  number_formatting() 

over500 <-  property_size(over500)

# use sqm else sqft_m
over500$Size <- over500$sqm
over500$Size[is.na(over500$sqm)] <- over500$sqft_m[is.na(over500$sqm)]

# room calculatino 
over500 <-  total_area_by_room(over500)

# use m values else ft values 
over500$TotalArea <- over500$TotalArea_m
over500$TotalArea[is.na(over500$TotalArea_m)] <- over500$TotalArea_ft_m[is.na(over500$TotalArea_m)]

# add total area to missing size 
over500$Size[is.na(over500$Size)] <- over500$TotalArea[is.na(over500$Size)]



# use total area for the size over 1000
indices <- which(over500$Size > 1000)
over500$Size[indices] <- over500$TotalArea[indices]

# fix specific entries 
over500 <- over500 %>%
  mutate(Size = if_else(Id == 602517, Size * 10.973, Size)) %>%
  mutate(Size = if_else(Id == 611928, TotalArea, Size)) %>%
  mutate(Size = if_else(Id == 561465, Size/10.973, Size))

# all under 80 should use totalarea if no missing 
indices <- which(over500$Size < 80.1 & (over500$TotalArea > over500$Size))
over500$Size[indices] <- over500$TotalArea[indices]

# one under 40 value to be removed 
over500$Size[over500$Size < 40] <- NA

# update the values in hd$Size 
hd$Size[hd$Id %in% over500$Id] <- over500$Size[hd$Id %in% over500$Id]




#### Missing Values  

na_size <- hd[is.na(hd$Size), ]

# Remove NA Size for now 
hd <- hd[!is.na(hd$Size), ]

# ggplot(hd, aes(x = Size, y= Price)) +geom_text(aes(label=Id))

# fix outliers 
hd <- hd[! hd$Id %in% c(616459, 616499, 540100, 528412), ]

# Fix Specific Entries 
hd$Size[hd$Id == 624939] <- 146.48
hd$Size[hd$Id == 623620] <- 132
hd$Baths[hd$Id == 532443] <- 1
hd$Baths[hd$Id == 513682] <- 2; hd$Beds[hd$Id == 513682] <- 2
hd$Beds[hd$Id == 615680] <- 10
hd <- hd[hd$Id != 621333, ]



#### ********* ********* ********* CHECKPOINT ********* ********* ********* ####
# this hd now has no missing values for bedrooms and bathrooms, property type and size
write.csv(hd, './Data/hd_textmined_temp.csv', row.names = F)  # save temp 
hd <- readr::read_csv('./Data/hd_textmined_temp.csv')



## THESE OCCUR HERE 
# ### Create all shapefiles 
# source(here("./Code/sr3_mapping.R"))
# 
# # run code to create subareas 
# source('./Code/sr4_create_subareas.R')








##-----------------------------------------------------------------------------
# Text Mining: BER 
##-----------------------------------------------------------------------------


# 838 missing BER values 
no_ber <- hd[is.na(hd$Ber), ]

# input g for ber exempt 
ber_exempt <- grepl("\\bber\\b.*\\bexempt\\b", no_ber$Description, ignore.case = T)
hd$Ber[hd$Id %in% no_ber$Id[ber_exempt]] <- 'G'

# input o for no ber  
no_ber_info <- grepl("ber.*(to follow|awaiting|to be confirmed|on order|Awaited|to be verified)|awaiting ber", no_ber$Description, ignore.case = T)
hd$Ber[hd$Id %in% no_ber$Id[no_ber_info]] <- 'o'

# post ber as char digit 
no_ber <- hd[is.na(hd$Ber), ]
post_pattern <- "(?i)ber.*([a-gA-G]\\d)"
out <- str_match(no_ber$Description, post_pattern)[,2]
hd$Ber[hd$Id %in% no_ber$Id] <- toupper(out)

# post ber as char only 
no_ber <- hd[is.na(hd$Ber), ]
post_pattern_nonum <- "(?i)\\bber\\b.*\\s([a-gA-G]).*"
out <- str_match(no_ber$Description, post_pattern_nonum)[,2]
hd$Ber[hd$Id %in% no_ber$Id] <- toupper(out)

# Manually Enter those with 'energy ratings'
hd$Ber[hd$Id == 597351] <- 'F'
hd$Ber[hd$Id %in% c(625857, 605502, 538379)] <- 'A2'
hd$Ber[hd$Id %in% c(620420, 617144, 617144)] <- 'A2'
hd$Ber[hd$Id == 572212] <- 'B'

no_ber <- hd[is.na(hd$Ber), ]

# remove the values with no ber 
hd <- hd[!(hd$Id %in% no_ber$Id), ]

# change single letters to the midle of that scale 
hd$Ber[hd$Ber =='A'] <- 'A2'
hd$Ber[hd$Ber =='B'] <- 'B2'
hd$Ber[hd$Ber =='C'] <- 'C2'
hd$Ber[hd$Ber =='E'] <- 'E2'
# remove the o values there is no information 
hd <- hd[!(hd$Ber == 'o'), ]




##-----------------------------------------------------------------------------
# Text Mining: Dummy Varaibales 
##-----------------------------------------------------------------------------


# remove the old ones 
hd <- dplyr::select(hd, -c('HasGarden', 'HasParking', 'HasGroundFloor', 'HasDevelopmentOpportunity', 'IsSouthFacing', 'IsPenthouse', 'IsApartment', 'IsUpdated', 'IsAttic', 'IsDated'))

# Define the patterns for each one 
attic_pttn <- "(?i)attic\\s*(-)?\\s*conversion|attic\\s*(-)?\\s*converted"
garden_pttn <- "garden|lawn"
cds_pttn <- "cul\\s*(-)?\\s*de\\s*(-)?\\s*sac|cul the sac|cul des ac|cul dec sac|de sac"
garage_pttn <- "barn|lean\\s*-?\\s*to|leanto|\\bgarage|\\bshed|\\bout\\s*(-)?\\s*building|outbuilding|outhouse|\\bout\\s*-?\\s*house"
renovated_pttn <- 'renovated|refurbished|turn\\s*(-)\\s*key|upgraded|walk\\s*(-)?\\s*condition'
new_pttn <- 'new\\s*(-)?\\s*build|new\\s*(-)?\\s*property|new\\s*(-)?\\s*development|\\bnew\\b'
period_pttn <- 'georgian|victorian|edwardian|period\\s*(-)?\\s*property|period\\s*(-)?\\s*residence|period\\s*(-)?\\s*home'
south_pttn <- 'south\\s*(-)*\\s*facing|southern\\s*(-)*\\s*facing'
ground_floor_pptn <- 'ground\\s*(-)?\\s*floor'
second_floor_pptn <- 'second\\s*(-)?\\s*floor'
penthouse_pttn <- 'pent\\s*(-)?\\s*house'

# Search the index for each pattern 
attic_inx <- grepl(attic_pttn, hd$Description, ignore.case = TRUE)
garden_inx <- grepl(garden_pttn, hd$Description, ignore.case = TRUE)
cds_inx <- grepl(cds_pttn, hd$Description, ignore.case = TRUE)
garage_inx <- grepl(garage_pttn, hd$Description, ignore.case = TRUE)
renovated_inx <- grepl(renovated_pttn, hd$Description, ignore.case = TRUE)
new_inx <- grepl(new_pttn, hd$Description, ignore.case = TRUE)
period_inx <- grepl(period_pttn, hd$Description, ignore.case = TRUE)
south_inx <- grepl(south_pttn, hd$Description, ignore.case = TRUE)
ground_flr_apt_inx <- grepl(ground_floor_pptn, hd$Description, ignore.case = TRUE)
second_flr_apt_inx <- grepl(second_floor_pptn, hd$Description, ignore.case = TRUE)
penthouse_apt_inx <- grepl(penthouse_pttn, hd$Description, ignore.case = TRUE)

# adjust the apartment indexes to be false for houses 
ground_flr_apt_inx <- ifelse(hd$PropertyType == 'Apartment'|hd$PropertyType=='Duplex', ground_flr_apt_inx, FALSE)
second_flr_apt_inx <- ifelse(hd$PropertyType == 'Apartment'|hd$PropertyType=='Duplex', second_flr_apt_inx, FALSE)
penthouse_apt_inx <- ifelse(hd$PropertyType == 'Apartment'|hd$PropertyType=='Duplex', penthouse_apt_inx, FALSE)

# add the variables to the data 
hd$HasAttic <- attic_inx
hd$HasGarden <- garden_inx
hd$HasCulDeSac <- cds_inx
hd$HasGarage <- garage_inx
hd$IsRenovated <- renovated_inx
hd$IsPeriod <- period_inx 
hd$IsSouthFacing <- south_inx 
hd$IsGroundFlrApt <- ground_flr_apt_inx
hd$IsSecondFlrApt <- second_flr_apt_inx
hd$IsPenthouseApt <- penthouse_apt_inx


## Replace Property Condition with IsNew  
hd$IsNew <- hd$PropertyCondition == 'New' | new_inx
hd <- dplyr::select(hd, -c('PropertyCondition'))



