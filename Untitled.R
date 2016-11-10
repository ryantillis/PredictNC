library(dplyr)
library(data.table)

#statewide <- fread("statewide.csv", header = TRUE, data.table = TRUE)
#Separating Voted and not-voted

c(1581335,1595171,34000)

p1 <- read.csv("percents.csv")
p2 <- read.csv("percents2.csv")
p2 <- as.data.table(p2)
p2 <- melt(p2)
setorder(p2, variable)



not_voted <- statewide[!ncid %in% ABS2016$ncid]
voted <- ABS2016

voted[,ballot_mail_zip:=NULL]
voted[,ballot_mail_street_address:=NULL]
voted[,ballot_mail_city:=NULL]
voted[,ballot_mail_state:=NULL]
voted[,other_mail_addr1:=NULL]
voted[,other_mail_addr2:=NULL]
voted[,other_city_state_zip:=NULL]

voted[, DEM := 0]
voted[, REP := 0]
voted[, UNA := 0]
voted[, LIB := 0]
voted[voter_party_code=="DEM", DEM := 1]
voted[voter_party_code=="REP", REP := 1]
voted[voter_party_code=="UNA", UNA := 1]
voted[voter_party_code=="LIB", LIB := 1]

voted[, WHITE := 0]
voted[, BLACK := 0]
voted[, ASIAN := 0]
voted[, OTHER := 0]
voted[, MIX := 0]
voted[, UNDESIGNATED := 0]
voted[, NATIVE := 0]

voted[race=="WHITE", WHITE := 1]
voted[race=="BLACK or AFRICAN AMERICAN", BLACK := 1]
voted[race=="ASIAN", ASIAN := 1]
voted[race=="OTHER", OTHER := 1]
voted[race=="TWO or MORE RACES", MIX := 1]
voted[race=="UNDESIGNATED", UNDESIGNATED := 1]
voted[race=="INDIAN AMERICAN or ALASKA NATIVE", NATIVE := 1]
voted[race=="'UNDESIGNATED", UNDESIGNATED := 1]

voted[, MALE := 0]
voted[, FEMALE := 0]

voted[gender=="M", MALE := 1]
voted[gender=="F", FEMALE := 1]

voted[, "18_29" := 0]
voted[, "30_44" := 0]
voted[, "45_64" := 0]
voted[, Over65 := 0]

voted[age>=65, Over65 := 1]
voted[age >=18 & age <=29, "18_29" := 1]
voted[age >=30 & age <=44, "30_44" := 1]
voted[age >=45 & age <=64, "45_64" := 1]

agg_voted_county <- voted %>% group_by(county_desc) %>% summarise_each(funs(sum),DEM,REP,UNA,LIB)

agg_voted_party <- voted %>% group_by(ballot_request_party) %>% summarise_each(funs(sum),WHITE,BLACK,ASIAN,OTHER,MIX,UNDESIGNATED,NATIVE,MALE,FEMALE,Over65,`18_29`, `30_44`, `45_64`)

long_avp <- melt(agg_voted_party)

agg_voted <- voted %>% summarise_each(funs(sum),DEM,REP,UNA,LIB,WHITE,BLACK,ASIAN,OTHER,MIX,UNDESIGNATED,NATIVE,MALE,FEMALE,Over65,`18_29`, `30_44`, `45_64`)

long_agg <- melt(agg_voted)

#Creating likely voter profiles
statewide[,voter_status_desc := NULL]
statewide[,status_cd := NULL]
statewide[,county_id := NULL]
statewide[,voter_status_reason_desc := NULL]
statewide[name_prefx_cd := NULL]
statewide[,name_suffix_lbl := NULL]
statewide[,first_name := NULL]
statewide[,last_name := NULL]
statewide[,middle_name := NULL]
statewide[,name_prefx_cd := NULL]
statewide[,name_suffix_lbl := NULL]
statewide[,res_street_address := NULL]
statewide[,res_city_desc := NULL]
statewide[,statewide := NULL]
statewide[,zip_code:= NULL]
statewide[,mail_addr1 := NULL]
statewide[,mail_addr2 := NULL]
statewide[,mail_addr3 := NULL]
statewide[,mail_addr4 := NULL]
statewide[,mail_city := NULL]
statewide[,mail_state := NULL]
statewide[,mail_zipcode := NULL]
statewide[,full_phone_number := NULL]
statewide[,municipality_abbrv := NULL]
statewide[,municipality_desc := NULL]
statewide[,ward_abbrv := NULL]
statewide[,ward_desc := NULL]
statewide[, county_commiss_abbrv := NULL]
statewide[, county_commiss_desc := NULL]
statewide[, township_abbrv := NULL]
statewide[, township_desc := NULL]
statewide[, school_dist_abbrv := NULL]
statewide[, school_dist_desc := NULL]
statewide[, fire_dist_abbrv := NULL]
statewide[, fire_dist_desc := NULL]
statewide[, water_dist_abbrv := NULL]
statewide[, water_dist_desc := NULL]
statewide[, sewer_dist_abbrv := NULL]
statewide[, sewer_dist_desc := NULL]
statewide[, sanit_dist_abbrv := NULL]
statewide[, sanit_dist_desc := NULL]
statewide[, rescue_dist_abbrv := NULL]
statewide[, rescue_dist_desc := NULL]
statewide[, munic_dist_abbrv := NULL]
statewide[, munic_dist_desc := NULL]
statewide[, dist_1_abbrv := NULL]
statewide[, dist_2_desc := NULL]
statewide[, confidential_ind := NULL]

#Creating likely voter profile from history 
NCBOE$voted_party_cd <- as.factor(NCBOE$voted_party_cd)
NCBOE$county_desc <- as.factor(NCBOE$county_desc)
NCBOE$election_lbl <- as.factor(NCBOE$election_lbl)
NCBOE$voting_method <- as.factor(NCBOE$voting_method)
NCBOE$election_desc <- as.factor(NCBOE$election_desc)

#Creating Relevant Indexes
pindex <- grep("PRIMARY",NCBOE$election_desc)
gindex <- grep("PRIMARY",NCBOE$election_desc)
index06 <- grep("/2006", NCBOE$election_lbl)
index07 <- grep("/2007", NCBOE$election_lbl)
index08 <- grep("/2008", NCBOE$election_lbl)
index09 <- grep("/2009", NCBOE$election_lbl)
index10 <- grep("/2010", NCBOE$election_lbl)
index11 <- grep("/2011", NCBOE$election_lbl)
index12 <- grep("/2012", NCBOE$election_lbl)
index13 <- grep("/2013", NCBOE$election_lbl)
index14 <- grep("/2014", NCBOE$election_lbl)
index15 <- grep("/2015", NCBOE$election_lbl)
index16 <- grep("/2016", NCBOE$election_lbl)

#Setting Up columns in a Memory efficient manner
NCBOE[,y2006:=0]
NCBOE[,y2007:=0]
NCBOE[,y2008:=0]
NCBOE[,y2009:=0]
NCBOE[,y2010:=0]
NCBOE[,y2011:=0]
NCBOE[,y2012:=0]
NCBOE[,y2013:=0]
NCBOE[,y2014:=0]
NCBOE[,y2015:=0]

NCBOE[,REP:=0]
NCBOE[,DEM:=0]
NCBOE[,UNA:=0]
NCBOE[,LIB:=0]

NCBOE[,GEN:=0]
NCBOE[,PRIME:=0]

#Writing binary Variables For General / Primary Voters
NCBOE[gindex,GEN:=1]
NCBOE[pindex,PRIME:=1]

#Writing binary variables for Party affiliations
NCBOE[voted_party_cd=="REP",REP:=1]
NCBOE[voted_party_cd=="DEM",DEM:=1]
NCBOE[voted_party_cd=="UNA",UNA:=1]
NCBOE[voted_party_cd=="LIB",LIB:=1]

#Binary variables for year 
NCBOE[index06, y2006 := 1]
NCBOE[index07, y2007 := 1]
NCBOE[index08, y2008 := 1]
NCBOE[index09, y2009 := 1]
NCBOE[index10, y2010 := 1]
NCBOE[index11, y2011 := 1]
NCBOE[index12, y2012 := 1]
NCBOE[index13, y2013 := 1]
NCBOE[index14, y2014 := 1]
NCBOE[index15, y2015 := 1]


#Filling in the details
NCBOE <- mutate(NCBOE, 
                REP_PRIME = PRIME + REP, 
                DEM_PRIME = PRIME + DEM, 
                UNA_PRIME = PRIME + UNA, 
                LIB_PRIME = PRIME +LIB,
                REP_GEN = GEN + REP, 
                DEM_GEN = GEN + DEM, 
                UNA_GEN = GEN + UNA, 
                LIB_GEN = GEN +LIB,
                REP_GEN_08 = REP_GEN + y2008, 
                DEM_GEN_08 = DEM_GEN + y2008, 
                UNA_GEN_08 = UNA_GEN + y2008, 
                LIB_GEN_08 = LIB_GEN + y2008,
                REP_GEN_09 = REP_GEN + y2009, 
                DEM_GEN_09 = DEM_GEN + y2009, 
                UNA_GEN_09 = UNA_GEN + y2009,
                LIB_GEN_09 = LIB_GEN + y2009,
                REP_GEN_10 = REP_GEN + y2010, 
                DEM_GEN_10 = DEM_GEN + y2010, 
                UNA_GEN_10 = UNA_GEN + y2010, 
                LIB_GEN_10 = LIB_GEN + y2010,
                REP_GEN_11 = REP_GEN + y2011,
                DEM_GEN_11 = DEM_GEN + y2011,
                UNA_GEN_11 = UNA_GEN + y2011,
                LIB_GEN_11 = LIB_GEN + y2011,
                REP_GEN_12 = REP_GEN + y2012,
                DEM_GEN_12 = DEM_GEN + y2012,
                UNA_GEN_12 = UNA_GEN + y2012,
                LIB_GEN_12 = LIB_GEN + y2012,
                REP_GEN_13 = REP_GEN + y2013,
                DEM_GEN_13 = DEM_GEN + y2013,
                UNA_GEN_13 = UNA_GEN + y2013,
                LIB_GEN_13 = LIB_GEN + y2013,
                REP_GEN_14 = REP_GEN + y2014,
                DEM_GEN_14 = DEM_GEN + y2014,
                UNA_GEN_14 = UNA_GEN + y2014,
                LIB_GEN_14 = LIB_GEN + y2014,
                REP_GEN_15 = REP_GEN + y2015,
                DEM_GEN_15 = DEM_GEN + y2015,
                UNA_GEN_15 = UNA_GEN + y2015,
                LIB_GEN_15 = LIB_GEN + y2015,
                REP_GEN_06 = REP_GEN + y2006,
                DEM_GEN_06 = DEM_GEN + y2006,
                UNA_GEN_06 = UNA_GEN + y2006,
                LIB_GEN_06 = LIB_GEN + y2006,
                REP_GEN_07 = REP_GEN + y2007,
                DEM_GEN_07 = DEM_GEN + y2007,
                UNA_GEN_07 = UNA_GEN + y2007,
                LIB_GEN_07 = LIB_GEN + y2007)

NCBOE <- as.data.table(NCBOE)

#Resetting values to binary
NCBOE[REP_GEN==1, REP_GEN:=0]
NCBOE[DEM_GEN==1, DEM_GEN:=0]
NCBOE[UNA_GEN==1, UNA_GEN:=0]
NCBOE[LIB_GEN==1, LIB_GEN:=0]

NCBOE[REP_PRIME==1, REP_PRIME:=0]
NCBOE[DEM_PRIME==1, DEM_PRIME:=0]
NCBOE[UNA_PRIME==1, UNA_PRIME:=0]
NCBOE[LIB_PRIME==1, LIB_PRIME:=0]

NCBOE[REP_GEN==2, REP_GEN:=1]
NCBOE[DEM_GEN==2, DEM_GEN:=1]
NCBOE[UNA_GEN==2, UNA_GEN:=1]
NCBOE[LIB_GEN==2, LIB_GEN:=1]

NCBOE[REP_PRIME==2, REP_PRIME:=1]
NCBOE[DEM_PRIME==2, DEM_PRIME:=1]
NCBOE[UNA_PRIME==2, UNA_PRIME:=1]
NCBOE[LIB_PRIME==2, LIB_PRIME:=1]

rm(list=ls()[1:11])
rm(pindex)
gc()

agg <- NCBOE %>% group_by(ncid) %>% summarise_each(funs(sum),REP_PRIME, 
                                                   DEM_PRIME, 
                                                   UNA_PRIME, 
                                                   LIB_PRIME, 
                                                   REP_GEN, 
                                                   DEM_GEN, 
                                                   UNA_GEN, 
                                                   LIB_GEN, 
                                                   REP_GEN_08, 
                                                   DEM_GEN_08,
                                                   UNA_GEN_08, 
                                                   LIB_GEN_08, 
                                                   REP_GEN_09, 
                                                   DEM_GEN_09, 
                                                   UNA_GEN_09, 
                                                   LIB_GEN_09, 
                                                   REP_GEN_10, 
                                                   DEM_GEN_10, 
                                                   UNA_GEN_10, 
                                                   LIB_GEN_10, 
                                                   REP_GEN_11, 
                                                   DEM_GEN_11, 
                                                   UNA_GEN_11, 
                                                   LIB_GEN_11, 
                                                   REP_GEN_12, 
                                                   DEM_GEN_12, 
                                                   UNA_GEN_12, 
                                                   LIB_GEN_12, 
                                                   REP_GEN_13, 
                                                   DEM_GEN_13, 
                                                   UNA_GEN_13, 
                                                   LIB_GEN_13, 
                                                   REP_GEN_14, 
                                                   DEM_GEN_14, 
                                                   UNA_GEN_14, 
                                                   LIB_GEN_14, 
                                                   REP_GEN_15, 
                                                   DEM_GEN_15, 
                                                   UNA_GEN_15, 
                                                   LIB_GEN_15, 
                                                   REP_GEN_06, 
                                                   DEM_GEN_06, 
                                                   UNA_GEN_06, 
                                                   LIB_GEN_06, 
                                                   REP_GEN_07, 
                                                   DEM_GEN_07, 
                                                   UNA_GEN_07, 
                                                   LIB_GEN_07) 

row_sums <- apply(agg, 2, sum)