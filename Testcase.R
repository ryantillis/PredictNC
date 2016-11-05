#Test case
abs_11_08_2011 <- read.csv("absentee11xx08xx2011.csv")
head(abs_11_08_2011)
names(abs_11_08_2011)
unique(abs_11_08_2011$ballot_req_type)
unique(abs_11_08_2011$precinct_desc)
unique(abs_11_08_2011$ballot_request_party)

#Assigning default binary value of 0 
abs_11_08_2011$DEM <- c(0)
abs_11_08_2011$REP <- c(0)
abs_11_08_2011$UNA <- c(0)
abs_11_08_2011$LIB <- c(0)

#Creating Dummy Variables for Party Affiliations will allow for easy aggregation and analysis
REP_index <- grep("REP",abs_11_08_2011$ballot_request_party)
DEM_index <- grep("DEM",abs_11_08_2011$ballot_request_party)
UNA_index <- grep("UNA",abs_11_08_2011$ballot_request_party)
LIB_index <- grep("LIB",abs_11_08_2011$ballot_request_party)

#Assigning binary value 1 for party affiliation
abs_11_08_2011$REP[REP_index] <- 1
abs_11_08_2011$DEM[DEM_index] <- 1
abs_11_08_2011$UNA[UNA_index] <- 1
abs_11_08_2011$LIB[LIB_index] <- 1

#Binary Variables for race
abs_11_08_2011$WHITE <- c(0)
abs_11_08_2011$BLACK <- c(0)
abs_11_08_2011$NATIVE <- c(0)
abs_11_08_2011$UND <- c(0)
abs_11_08_2011$TWO <- c(0)
abs_11_08_2011$ASIAN <- c(0)

W_index <- grep("WHITE",abs_11_08_2011$race)
B_index <- grep("*BLACK*",abs_11_08_2011$race)
N_index <- grep("*NATIVE*",abs_11_08_2011$race)
U_index <- grep("*UND*",abs_11_08_2011$race)
T_index <- grep("*TWO*",abs_11_08_2011$race)
A_index <- grep("ASIAN",abs_11_08_2011$race)


abs_11_08_2011$WHITE[W_index] <- 1
abs_11_08_2011$BLACK[B_index] <- 1
abs_11_08_2011$NATIVE[N_index] <- 1
abs_11_08_2011$UND[U_index] <- 1
abs_11_08_2011$TWO[T_index] <- 1
abs_11_08_2011$ASIAN[A_index] <- 1


#Aggregating and merging the data and writing to csv
agg_party <- abs_11_08_2011 %>% group_by(county_desc) %>% summarise_each(funs(sum), DEM, REP, UNA, LIB)
agg_age <- abs_11_08_2011 %>% group_by(county_desc) %>% summarise_each(funs(mean), age)
agg_data <- left_join(agg_party,agg_age)
agg_race <- abs_11_08_2011 %>% group_by(county_desc) %>% summarise_each(funs(sum), WHITE, BLACK, NATIVE, UND, TWO, ASIAN)
agg_data <- left_joint(agg_data,agg_race)
