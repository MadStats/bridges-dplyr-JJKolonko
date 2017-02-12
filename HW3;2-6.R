install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("readr")

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)

# I like downloading straight from url's.  
# If I was planning to share this code in a long time, I would worry about stability of the accessibility (url/data/format) and maybe backup locally.
# For class, downloading is straightforward.

# I like fread (from data.table) and read_csv (from readr).  
# in my experience, fread is faster and deals with data entry errors a little more elegantly.
# downside of fread: it saves to format data.table.  
#   if you know how to use it, this can be super-duper fast.
# we will be using dplyr instead, which is merely super fast. 
# So, I always convert to a tibble (as.tbl) after an fread.  tibbles are basically data.frames that print nicer.

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt" #can only go between 2010-2016
tmp = fread(dest) 
tmp = as.tbl(tmp)
tmp1 = read_csv(dest)
tmp2 = read_csv(dest, col_types = "c")  # could make them all characters...
classes = sapply(tmp, class)


states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()


# this is one possibility...
# for(i in 2:51){
#   dest=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
#   dat[[i]] = fread(dest, colClasses = classes ) %>% as.tbl  # for loop to define data could be really bad.  Does list make it ok?  idk.
# }
# lapply(dat, dim)
# colnames(dat[[1]])


# # gosh, it would be nice if these worked!
# x = fread("https://www.fhwa.dot.gov/bridge/nbi/2016allstatesallrecsdel.zip")
# x = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2016allstatesallrecsdel.zip")


# Hadley typically has good stuff! 
# his code chunk from ftp://cran.r-project.org/pub/R/web/packages/tidyr/vignettes/tidy-data.html
# library(plyr)
# paths <- dir("data", pattern = "\\.csv$", full.names = TRUE)
# names(paths) <- basename(paths)
# ldply(paths, read.csv, stringsAsFactors = FALSE)




dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  


save(x16, file = "allStates16.RData")
# let's dig into the values and see if there are any crazy things going on...
M = x16
# M = M[,-14]
is.na(M) %>% rowSums %>% hist #hist(rowSums(is.na(M))) is equivalent
#fills the first argument of the next function
is.na(M) %>% colSums %>% hist(breaks = 100)
fun = function(x){ return(which(x>20)) }
(bad =  is.na(M) %>% colSums %>% fun)
M = M[,-bad]
jold =1
for(j in jold:ncol(M)){
  nc = nchar(M[,j], keepNA = T)
  print(j)
  print(summary(nc))
  print(sum(is.na(nc)))
  print("")
}
colnames(M)[j]
M = M[,-j]
jold = j

colnames(M)
# [1] "STATE_CODE_001"          "STRUCTURE_NUMBER_008"    "RECORD_TYPE_005A"        "ROUTE_PREFIX_005B"       "SERVICE_LEVEL_005C"      "ROUTE_NUMBER_005D"       "DIRECTION_005E"          "HIGHWAY_DISTRICT_002"   
# [9] "COUNTY_CODE_003"         "PLACE_CODE_004"          "FEATURES_DESC_006A"      "FACILITY_CARRIED_007"    "MIN_VERT_CLR_010"        "KILOPOINT_011"           "LRS_INV_ROUTE_013A"      "LAT_016"                
# [17] "LONG_017"                "DETOUR_KILOS_019"        "TOLL_020"                "MAINTENANCE_021"         "OWNER_022"               "FUNCTIONAL_CLASS_026"    "YEAR_BUILT_027"          "TRAFFIC_LANES_ON_028A"  
# [25] "TRAFFIC_LANES_UND_028B"  "ADT_029"                 "YEAR_ADT_030"            "DESIGN_LOAD_031"         "APPR_WIDTH_MT_032"       "MEDIAN_CODE_033"         "DEGREES_SKEW_034"        "STRUCTURE_FLARED_035"   
# [33] "RAILINGS_036A"           "TRANSITIONS_036B"        "APPR_RAIL_036C"          "APPR_RAIL_END_036D"      "HISTORY_037"             "NAVIGATION_038"          "NAV_VERT_CLR_MT_039"     "NAV_HORR_CLR_MT_040"    
# [41] "OPEN_CLOSED_POSTED_041"  "SERVICE_ON_042A"         "SERVICE_UND_042B"        "STRUCTURE_KIND_043A"     "STRUCTURE_TYPE_043B"     "APPR_KIND_044A"          "APPR_TYPE_044B"          "MAIN_UNIT_SPANS_045"    
# [49] "APPR_SPANS_046"          "HORR_CLR_MT_047"         "MAX_SPAN_LEN_MT_048"     "STRUCTURE_LEN_MT_049"    "LEFT_CURB_MT_050A"       "RIGHT_CURB_MT_050B"      "ROADWAY_WIDTH_MT_051"    "DECK_WIDTH_MT_052"      
# [57] "VERT_CLR_OVER_MT_053"    "VERT_CLR_UND_REF_054A"   "VERT_CLR_UND_054B"       "LAT_UND_REF_055A"        "LAT_UND_MT_055B"         "LEFT_LAT_UND_MT_056"     "DECK_COND_058"           "SUPERSTRUCTURE_COND_059"
# [65] "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"        "OPR_RATING_METH_063"     "INV_RATING_METH_065"     "STRUCTURAL_EVAL_067"     "DECK_GEOMETRY_EVAL_068"  "UNDCLRENCE_EVAL_069"    
# [73] "POSTING_EVAL_070"        "WATERWAY_EVAL_071"       "APPR_ROAD_EVAL_072"      "DATE_OF_INSPECT_090"     "FRACTURE_092A"           "UNDWATER_LOOK_SEE_092B"  "SPEC_INSPECT_092C"       "STRAHNET_HIGHWAY_100"   
# [81] "PARALLEL_STRUCTURE_101"  "TRAFFIC_DIRECTION_102"   "HIGHWAY_SYSTEM_104"      "FEDERAL_LANDS_105"       "DECK_STRUCTURE_TYPE_107" "SURFACE_TYPE_108A"       "MEMBRANE_TYPE_108B"      "DECK_PROTECTION_108C"   
# [89] "NATIONAL_NETWORK_110"    "BRIDGE_LEN_IND_112"      "SCOUR_CRITICAL_113"      "FUTURE_ADT_114"          "YEAR_OF_FUTURE_ADT_115"  "FED_AGENCY"              "DATE_LAST_UPDATE"        "TYPE_LAST_UPDATE"       
# [97] "DEDUCT_CODE"             "PROGRAM_CODE"            "PROJ_NO"                 "STATUS_WITH_10YR_RULE"   "SUFFICIENCY_ASTERC"      "SUFFICIENCY_RATING"      "STATUS_NO_10YR_RULE"    

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029", "YEAR_ADT_030" ,
         "YEAR_BUILT_027", "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061", "CULVERT_COND_062",
         "DECK_STRUCTURE_TYPE_107",  "SURFACE_TYPE_108A", "MEMBRANE_TYPE_108B", "DECK_PROTECTION_108C")

# x = M[,match(keep, colnames(M))]
M = as.tbl(M)
x = select(M, one_of(keep))  # see chapter 5 (section 4) in r4ds.


library(ggplot2)
#ggplot(data = M) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))
#M = filter(M,LONG_017 > 0)
#ggplot(data = M) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))

min2dec = function(x){
  substr(x,3,8) %>% return
}
hist(M$LAT_016 %>% min2dec %>% as.numeric)

min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
min2dec(M$LAT_016[1])
hist(M$LAT_016 %>% min2dec %>% as.numeric)

M = mutate(M,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
ggplot(data = M) +geom_point(mapping = aes(y = lat, x = lon))

#M = filter(M,lon<100)
#ggplot(data = M) +geom_point(mapping = aes(y = lat, x = lon))

#ggplot(data = M) +geom_point(mapping = aes(y = lat, x = lon,col =TOLL_020))
#ggplot(data = M) +geom_point(mapping = aes(y = lat, x = lon,col =YEAR_BUILT_027))
#ggplot(data = M) +geom_point(mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027))
#ggplot(data = M, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027)) +geom_point() + geom_smooth()

#ggplot(data = M, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)


# make function to rate bridge as NA, good, bad, fail, using 
# colnames(M)[10:13]
# "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"  
# good = 5:9
# bad = 2:4
# fail = 0:1

# cond "condition" is the minimum of the given ratings. 
M = mutate(M, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                          na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

M$rate = rateIt(M$cond)
table(M$cond)
table(M$rate)
M = filter(M, cond>1)
#ggplot(data = M, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = rate)) +geom_point() + geom_smooth()

M %>% group_by(YEAR_BUILT_027) %>% summarise(prop = mean(rate=="good")) %>% ggplot(mapping = aes(YEAR_BUILT_027, y = prop)) + geom_point()


map = ggplot(data = M, mapping = aes(y = lat, x = lon))
#map + geom_point(aes(col=rate))+ scale_colour_brewer(palette = "Spectral")  

# where are these bad roads?!!??
ggplot(data = M, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()
colnames(M)


# use a data playground!

M = mutate(M, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
M = M %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)
M$fips %>% head

M = M %>% mutate(good = (rate == "good"))
table(M$good)
dat = M %>% group_by(fips) %>% summarize(propGoodRoads = mean(good))

# dat = M %>% group_by(good) %>% summarize(tmp = mean(lat))

dat %>% transmute(region = fips, value = propGoodRoads) %>% county_choropleth(state_zoom = "wisconsin")



#CODE--Copied pieces from above code, above code provided by Karl Rohe
#look at wearing surface (item 108A)
#check the proportion of the bridges that do not have a deck, denoted "N"
#wi = filter(x, STATE_CODE_001 == 55) check to run around line 131
Q = M
Q = filter(Q, SURFACE_TYPE_108A != "N") #remove "not applicable"
Q = filter(Q, SURFACE_TYPE_108A != "9") #remove "other
#ggplot(data = Q) +geom_point(mapping = aes(y = lat, x = lon, col =SURFACE_TYPE_108A)) + scale_x_reverse()

#this does not look helpful: listed as "unknown"
#Q = filter(Q, MEMBRANE_TYPE_108B != "N") #remove "not applicable"
#Q = filter(Q, MEMBRANE_TYPE_108B != "0") #remove "none"
#Q = filter(Q, MEMBRANE_TYPE_108B != "9") #remove "other
#ggplot(data = Q) +geom_point(mapping = aes(y = lat, x = lon, col =MEMBRANE_TYPE_108B)) + scale_x_reverse()

#Q = filter(Q, DECK_PROTECTION_108C != "N") #remove "not applicable"
#Q = filter(Q, DECK_PROTECTION_108C != "9") #remove "other
#ggplot(data = Q) +geom_point(mapping = aes(y = lat, x = lon, col =DECK_PROTECTION_108C)) + scale_x_reverse()

#compare "toll road type" to "deck condition" --Do not use WI for this--
#ggplot(data = Q, mapping = aes(y = TOLL_020, x = DECK_COND_058)) +geom_point() + geom_smooth(method = "loess", span = .7)

#compare "avg daily trafic" to "deck condition"
#ggplot(data = Q, mapping = aes(y = ADT_029, x = DECK_COND_058, col = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)

#compare "avg daily trafic" to "surface type"
#ggplot(data = wi, mapping = aes(y = ADT_029, x = SURFACE_TYPE_108A, col = DECK_COND_058)) +geom_point() + geom_smooth(method = "loess", span = .7)

#compare "deck protection" to "deck rating"
#Q = filter(Q, DECK_COND_058 != "N")
#table(Q$DECK_PROTECTION_108C, Q$DECK_COND_058)
#ggplot(data = Q, mapping = aes(y = DECK_PROTECTION_108C, x = DECK_COND_058, col = YEAR_BUILT_027)) +geom_point() + geom_smooth(method = "loess", span = .7) + geom_jitter()
#Looks like Epoxy Coated Reinforcing (Protection=1) is better than no protection (Protection=0)

#compare "year built" to the rating
#ggplot(data = Q, mapping = aes(y = YEAR_BUILT_027, x = DECK_COND_058)) +geom_point() + geom_smooth(method = "loess", span = .7)
#ggplot(data = Q, mapping = aes(y = YEAR_BUILT_027, x = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)
#ggplot(data = Q, mapping = aes(y = YEAR_BUILT_027, x = SUBSTRUCTURE_COND_060)) +geom_point() + geom_smooth(method = "loess", span = .7)
#ggplot(data = Q, mapping = aes(y = YEAR_BUILT_027, x = CHANNEL_COND_061)) +geom_point() + geom_smooth(method = "loess", span = .7)
#ggplot(data = Q, mapping = aes(y = YEAR_BUILT_027, x = CULVERT_COND_062)) +geom_point() + geom_smooth(method = "loess", span = .7)
#bit of a trend: bridges built later are in better deck condition, not surprising...

#Look at distributions of condition ratings
#hist(as.numeric(Q$DECK_COND_058))
#hist(as.numeric(Q$SUPERSTRUCTURE_COND_059))
#hist(as.numeric(Q$SUBSTRUCTURE_COND_060))
#hist(as.numeric(Q$CHANNEL_COND_061))
#hist(as.numeric(Q$CULVERT_COND_062)) #all "not applicable" entries so from now on I will leave this factor out

#plot below works for states with tolls
#ggplot(data = Q, mapping = aes(y = YEAR_BUILT_027, x = TOLL_020, col = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)

#show most commonly used building material by county
names(sort(summary(as.factor(Q$SURFACE_TYPE_108A)), decreasing=T))
sTyp <- data.frame(Q$fips, Q$SURFACE_TYPE_108A)

library(tidyr)
library(dplyr)
sTyp %>% 
  gather(Q.fips, Q.SURFACE_TYPE_108A) %>%
  count(Q.fips, Q.SURFACE_TYPE_108A) -> sTypNew
sTypNew <- spread(sTypNew, key = Q.SURFACE_TYPE_108A, value = n, fill = 0)


surfTyp <- rep(0, length = nrow(sTypNew))
surfTyp <- as.numeric(colnames(sTypNew)[max.col(sTypNew[,2:9],ties.method="random")])
surfTyp <- surfTyp+1

final <- data.frame(sTypNew$Q.fips, surfTyp)
colnames(final) <- c("fips", "surfType")

final %>% transmute(region = fips, value = surfType) %>% county_choropleth() + scale_fill_discrete(name="Surface Type",
                                                                                                 breaks=c("1", "[2]", "NA"),
                                                                                                 labels=c("Monolithic Concrete", "Bituminous", "None"))




