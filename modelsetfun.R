modelset <- rbind(TOT06,TOT08,TOT10,TOT12,TOT14)

modelset$Libertarians <- gsub(",","",modelset$Libertarians)
modelset$Democrats <- gsub(",","",modelset$Democrats)
modelset$Republicans <- gsub(",","",modelset$Republicans)
modelset$Unaffiliated <- gsub(",","",modelset$Unaffiliated)
modelset$White <- gsub(",","",modelset$White)
modelset$Black <- gsub(",","",modelset$Black)
modelset$American.Indian <- gsub(",","",modelset$American.Indian)
modelset$Other <- gsub(",","",modelset$Other)
modelset$Hispanic <- gsub(",","",modelset$Hispanic)
modelset$Male <- gsub(",","",modelset$Male)
modelset$Female <- gsub(",","",modelset$Female)
modelset$Total <- gsub(",","",modelset$Total)

modelset[is.na(modelset)] <- 0

modelset$Democrats <- as.numeric(modelset$Democrats)
modelset$Republicans <- as.numeric(modelset$Republicans)
modelset$Libertarians <- as.numeric(modelset$Libertarians)
modelset$Unaffiliated <- as.numeric(modelset$Unaffiliated)
modelset$White <- as.numeric(modelset$White)
modelset$Black <- as.numeric(modelset$Black)
modelset$American.Indian <- as.numeric(modelset$American.Indian)
modelset$Other <- as.numeric(modelset$Other)
modelset$Hispanic <- as.numeric(modelset$Hispanic)
modelset$Male <- as.numeric(modelset$Male)
modelset$Female <- as.numeric(modelset$Female)
modelset$Total <- as.numeric(modelset$Total)

modelset[is.na(modelset)] <- 0

index06b <- grep("/2006", modelset$election_lbl)
index07b <- grep("/2007", modelset$election_lbl)
index08b <- grep("/2008", modelset$election_lbl)
index09b <- grep("/2009", modelset$election_lbl)
index10b <- grep("/2010", modelset$election_lbl)
index11b <- grep("/2011", modelset$election_lbl)
index12b <- grep("/2012", modelset$election_lbl)
index13b <- grep("/2013", modelset$election_lbl)
index14b <- grep("/2014", modelset$election_lbl)

modelset <- mutate(modelset, 
                   DOS_DEM = DOS/Democrats, 
                   ROS_REP = ROS/Republicans, 
                   UOS_UNA = UOS/Unaffiliated, 
                   LOS_LIB = LOS/Libertarians, 
                   DIP_DEM = DIP/Democrats, 
                   RIP_REP = RIP/Republicans, 
                   UIP_UNA = UIP/Unaffiliated, 
                   LIP_LIB = LIP/Libertarians, 
                   DBM_DEM = DBM/Democrats, 
                   RBM_REP = RBM/Republicans, 
                   UBM_UNA = UBM/Unaffiliated, 
                   LBM_LIB = LBM/Libertarians, 
                   DPR_DEM = DPR/Democrats, 
                   RPR_REP = RPR/Republicans, 
                   UPR_UNA = UPR/Unaffiliated,
                   LPR_LIB = LPR/Libertarians,
                   TOE_DEM = (DC+DPR+DIP)/Democrats, 
                   TOE_REP = (RC+RPR+RIP)/Republicans, 
                   TOE_UNA = (UC+UPR+UIP)/Unaffiliated, 
                   TOE_LIB = (LC+LPR+LIP)/Libertarians)

modelset[is.na(modelset)] <- 0

modelset <- as.data.table(modelset)

index06b <- grep("/2006", modelset$election_lbl) #house of reps and 1/3 of senate
index07b <- grep("/2007", modelset$election_lbl) #off
index08b <- grep("/2008", modelset$election_lbl) #president, governer, house, 1/3 Senate
index09b <- grep("/2009", modelset$election_lbl)#off
index10b <- grep("/2010", modelset$election_lbl)#House, 1/3 Senate
index11b <- grep("/2011", modelset$election_lbl)#off
index12b <- grep("/2012", modelset$election_lbl)#presidential, governer, H 1/3 S
index13b <- grep("/2013", modelset$election_lbl)#off
index14b <- grep("/2014", modelset$election_lbl)#House, 1/3 S
index15b <- grep("/2015", modelset$election_lbl)#off

modelset[,type:= "A"]

modelset[index06b, type:="F"]
modelset[index07b, type:="O"]
modelset[index08b, type:="P"]
modelset[index09b, type:="O"]
modelset[index10b, type:="F"]
modelset[index11b, type:="O"]
modelset[index12b, type:="P"]
modelset[index13b, type:="O"]
modelset[index14b, type:="F"]
modelset[index15b, type:="O"]

modelset$type <- as.factor(modelset$type)

DT <- as.data.table(modelset)
invisible(lapply(names(DT),function(.name) set(DT, which(is.infinite(DT[[.name]])), j = .name,value =0)))

modelset <- DT

train<-sample_frac(modelset, 0.8)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-modelset[-sid,]
head(train)