
aggregate_NC <- function(data){
       data <- read.table("absentee11xx07xx2006.csv", sep ="\t", header = TRUE)
       #data <- read.csv("absentee11xx08xx2016.csv")
       head(data)
       names(data)
       unique(data$ballot_req_type)
       unique(data$precinct_desc)
       unique(data$ballot_request_party)
       library(dplyr)
       
       #Assigning default binary value of 0 
       data$DEM <- c(0)
       data$REP <- c(0)
       data$UNA <- c(0)
       data$LIB <- c(0)
       
       #Creating Dummy Variables for Party Affiliations will allow for easy aggregation and analysis
       REP_index <- grep("REP",data$Ballot_request_party)
       DEM_index <- grep("DEM",data$Ballot_request_party)
       UNA_index <- grep("UNA",data$Ballot_request_party)
       LIB_index <- grep("LIB",data$Ballot_request_party)
       
       #Assigning binary value 1 for party affiliation
       data$REP[REP_index] <- 1
       data$DEM[DEM_index] <- 1
       data$UNA[UNA_index] <- 1
       data$LIB[LIB_index] <- 1
       
       #Binary Variables for race
       data$WHITE <- c(0)
       data$BLACK <- c(0)
       data$NATIVE <- c(0)
       data$UND <- c(0)
       data$TWO <- c(0)
       data$ASIAN <- c(0)
       
       W_index <- grep("WHITE",data$Race)
       B_index <- grep("*BLACK*",data$Race)
       N_index <- grep("*NATIVE*",data$Race)
       U_index <- grep("*UND*",data$Race)
       T_index <- grep("*TWO*",data$Race)
       A_index <- grep("ASIAN",data$Race)
       
       
       data$WHITE[W_index] <- 1
       data$BLACK[B_index] <- 1
       data$NATIVE[N_index] <- 1
       data$UND[U_index] <- 1
       data$TWO[T_index] <- 1
       data$ASIAN[A_index] <- 1
       
       
       #Aggregating and merging the data and writing to csv
       agg_party <- data %>% group_by(county_desc) %>% summarise_each(funs(sum), DEM, REP, UNA, LIB)
       agg_age <- data %>% group_by(county_desc) %>% summarise_each(funs(mean), age)
       agg_data <- left_join(agg_party,agg_age)
       agg_race <- data %>% group_by(county_desc) %>% summarise_each(funs(sum), WHITE, BLACK, NATIVE, UND, TWO, ASIAN)
       agg_data <- left_join(agg_data,agg_race)
