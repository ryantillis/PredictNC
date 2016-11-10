dat <- fread("NCBOE.csv", header = TRUE, data.table = TRUE)
#unique(dat$voted_party_cd)
dat$ballot_request_party <- as.factor(dat$ballot_request_party)
dat$county_desc <- as.factor(dat$county_desc)
dat$ballot_req_type <- as.factor(dat$ballot_req_type)

#selecting only accepted ballots there are only 57 "pending"
dat <- dat[ballot_rtn_status=="ACCEPTED",]

dat[, DEM := 0]
dat[, REP := 0]
dat[, UNA := 0]
dat[, LIB := 0]

#Note to NCBOE, store your variables as binary...it makes aggregation easier.
#Creating Dummy variables to aggregate party counts

dat[ballot_request_party=="REP", REP:=1]
dat[ballot_request_party=="DEM", DEM:=1]
dat[ballot_request_party=="UNA", UNA:=1]
dat[ballot_request_party=="LIB", LIB:=1]


VM <- unique(dat$voting_method)

#Could Perform all these actions as a loop but it is just as fast this way and more illustrative or what's going on
dat[, BM := 0]
dat[, OS := 0]


#Using simple summations and the dplyr package to create new dummy variables quickly

data <- mutate(dat, DOS = DEM + OS, ROS = REP + OS, UOS = UNA + OS, LOS = LIB + OS)
data <- mutate(data, DBM = DEM + BM, RBM = REP + BM, UBM = UNA + BM, LBM = LIB + BM)

data <- as.data.table(data)

#Changing 1's to 0's

data[DOS==1, DOS:=0]
data[ROS==1, ROS:=0]
data[LOS==1, LOS:=0]
data[UOS==1, UOS:=0]

data[DBM==1, DBM:=0]
data[RBM==1, RBM:=0]
data[UBM==1, UBM:=0]
data[LBM==1, LBM:=0]

#Changing 2's to 1's

data[DOS==2, DOS:=1]
data[ROS==2, ROS:=1]
data[LOS==2, LOS:=1]
data[UOS==2, UOS:=1]

data[DBM==2, DBM:=1]
data[RBM==2, RBM:=1]
data[UBM==2, UBM:=1]
data[LBM==2, LBM:=1]

aggregate_data <- data %>% group_by(county_desc) %>% summarise_each(funs(sum), DOS, ROS, UOS, LOS, DBM, RBM, UBM, LBM)

TOTAL2016 <- read.csv("2016TOTAL.csv")

TOTAL2016 <- as.data.table(TOTAL2016)

names(TOTAL2016)[1] <- c("county_desc")

TOTAL <- left_join(aggregate_data, TOTAL2016)

TOTAL$Libertarians <- gsub(",","",TOTAL$Libertarians)
TOTAL$Democrats <- gsub(",","",TOTAL$Democrats)
TOTAL$Republicans <- gsub(",","",TOTAL$Republicans)
TOTAL$Unaffiliated <- gsub(",","",TOTAL$Unaffiliated)
TOTAL$White <- gsub(",","",TOTAL$White)
TOTAL$Black <- gsub(",","",TOTAL$Black)
TOTAL$American.Indian <- gsub(",","",TOTAL$American.Indian)
TOTAL$Other <- gsub(",","",TOTAL$Other)
TOTAL$Hispanic <- gsub(",","",TOTAL$Hispanic)
TOTAL$Male <- gsub(",","",TOTAL$Male)
TOTAL$Female <- gsub(",","",TOTAL$Female)
TOTAL$Total <- gsub(",","",TOTAL$Total)

TOTAL$Democrats <- as.numeric(TOTAL$Democrats)
TOTAL$Republicans <- as.numeric(TOTAL$Republicans)
TOTAL$Libertarians <- as.numeric(TOTAL$Libertarians)
TOTAL$Unaffiliated <- as.numeric(TOTAL$Unaffiliated)
TOTAL$White <- as.numeric(TOTAL$White)
TOTAL$Black <- as.numeric(TOTAL$Black)
TOTAL$American.Indian <- as.numeric(TOTAL$American.Indian)
TOTAL$Other <- as.numeric(TOTAL$Other)
TOTAL$Hispanic <- as.numeric(TOTAL$Hispanic)
TOTAL$Male <- as.numeric(TOTAL$Male)
TOTAL$Female <- as.numeric(TOTAL$Female)
TOTAL$Total <- as.numeric(TOTAL$Total)

TOTAL <- mutate(TOTAL, DOS_DEM = DOS/Democrats, ROS_REP = ROS/Republicans, UOS_UNA = UOS/Unaffiliated, LOS_LIB = LOS/Libertarians, DBM_DEM = DBM/Democrats, RBM_REP = RBM/Republicans, UBM_UNA = UBM/Unaffiliated, LBM_LIB = LBM/Libertarians)

TOTAL <- as.data.table(TOTAL)
TOTAL[,type:="P"]

#Predicting against 2016 data
predict_2016 <- predict(model_all, TOTAL)

#model_all predicting 74.5% Democrat turnout
sum(predict_2016*TOTAL$Democrats+
           TOTAL$DOS_DEM*TOTAL$Democrats+
           TOTAL$DBM_DEM*TOTAL$Democrats)/(sum(TOTAL$Democrats))

#Model_all_dem1 predicting 69.26377%
predict_2016d <- predict(model_all_dem1, TOTAL)
sum(predict_2016d*TOTAL$Democrats+
           +         TOTAL$DOS_DEM*TOTAL$Democrats+
           +         TOTAL$DBM_DEM*TOTAL$Democrats)/(sum(TOTAL$Democrats))

#Model_all_rep1 predicting 74.0749% turnout
predict_2016r <- predict(model_all_rep1, TOTAL)
sum(predict_2016r*TOTAL$Republicans+
           TOTAL$ROS_REP*TOTAL$Republicans+
           TOTAL$RBM_REP*TOTAL$Republicans)/(sum(TOTAL$Republicans))

#Model_all_una1 predicting 62.16381% turnout
predict_2016u <- predict(model_all_una1, TOTAL)
sum(predict_2016u*TOTAL$Unaffiliated+
           TOTAL$UOS_UNA*TOTAL$Unaffiliated+
           TOTAL$UBM_UNA*TOTAL$Unaffiliated)/(sum(TOTAL$Unaffiliated))

#model_all_lib1 predicting 59.89222% turnout
predict_2016l <- predict(model_all_lib1, TOTAL)
sum(predict_2016l*TOTAL$Libertarians+
           TOTAL$LOS_LIB*TOTAL$Libertarians+
           TOTAL$LBM_LIB*TOTAL$Libertarians)/(sum(TOTAL$Libertarians))


#Model_all_dem1 predicting 69.26377%
predict_2016d <- predict(model_all_dem1, TOTAL)
sum()/(sum(TOTAL$Democrats))


sum(predict_2016r*TOTAL$Republicans+
           TOTAL$ROS_REP*TOTAL$Republicans+
           TOTAL$RBM_REP*TOTAL$Republicans+
           predict_2016u*TOTAL$Unaffiliated+
           TOTAL$UOS_UNA*TOTAL$Unaffiliated+
           TOTAL$UBM_UNA*TOTAL$Unaffiliated+
           predict_2016l*TOTAL$Libertarians+
           TOTAL$LOS_LIB*TOTAL$Libertarians+
           TOTAL$LBM_LIB*TOTAL$Libertarians+
           predict_2016d*TOTAL$Democrats+
           TOTAL$DOS_DEM*TOTAL$Democrats+
           TOTAL$DBM_DEM*TOTAL$Democrats)/(sum(TOTAL$Republicans+TOTAL$Unaffiliated+TOTAL$Libertarians+TOTAL$Democrats))

#total libs
sum(predict_2016l*TOTAL$Libertarians+
           +         TOTAL$LOS_LIB*TOTAL$Libertarians+
           +         TOTAL$LBM_LIB*TOTAL$Libertarians)
#total una
sum(predict_2016u*TOTAL$Unaffiliated+
             +         TOTAL$UOS_UNA*TOTAL$Unaffiliated+
             +         TOTAL$UBM_UNA*TOTAL$Unaffiliated)
#total Rep
sum(predict_2016r*TOTAL$Republicans+
             +         TOTAL$ROS_REP*TOTAL$Republicans+
                     TOTAL$RBM_REP*TOTAL$Republicans)

#total dem
sum(predict_2016d*TOTAL$Democrats+
             +         +         TOTAL$DOS_DEM*TOTAL$Democrats+
             +         +         TOTAL$DBM_DEM*TOTAL$Democrats)


#Total EV onsite
sum(TOTAL$LOS_LIB*TOTAL$Libertarians+TOTAL$UOS_UNA*TOTAL$Unaffiliated+TOTAL$ROS_REP*TOTAL$Republicans+TOTAL$DOS_DEM*TOTAL$Democrats)
#Total by mail
sum(TOTAL$LBM_LIB*TOTAL$Libertarians+TOTAL$UBM_UNA*TOTAL$Unaffiliated+TOTAL$RBM_REP*TOTAL$Republicans+TOTAL$DBM_DEM*TOTAL$Democrats)
#total election day
sum(predict_2016d*TOTAL$Democrats+predict_2016r*TOTAL$Republicans+predict_2016u*TOTAL$Unaffiliated+predict_2016l*TOTAL$Libertarians)



#Total Dems predicted by county
predict_2016*TOTAL$Democrats

#TOTAL democrats election day; 602990.5
tdem <- sum(predict_2016*TOTAL$Democrats)

#TOTAL onsite and by mail votes; 1294790
tea <- sum(TOTAL$DOS+TOTAL$DBM)


predict_2016 <- predict(model_occ_1, TOTAL)

#Total Dems predicted by county
predict_2016*TOTAL$Democrats

#TOTAL democrats election day; 602990.5
tdem <- sum(predict_2016*TOTAL$Democrats)

#TOTAL onsite and by mail votes; 1294790
tea <- sum(TOTAL$DOS+TOTAL$DBM)



#...need to go back and create a variable which is the sum or provisional, in person, and curbside

#Realized I added wrong, but still glad I went back...got a better model

#Predicted 75.4% off of only F and P years
#Predicting a 75.81% DEM turnout for model_all
#Predicting 75.07% DEM turnout for model_3
#Occ model produces 77% DEM turnout for model_occ_dem
(tdem+tea)/2715039

#So At this point I think the predictions are high, my best guess at the moment is that they overestimate the election-day turnout in counties with low population.


#Republican turnout is predicted to be 62.08% 
predict_2016*TOTAL$Republicans
trep <- sum(predict_2016*TOTAL$Republicans)
tear <- sum(TOTAL$ROS+TOTAL$RBM)
(trep+tear)/2715039
