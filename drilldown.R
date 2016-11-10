


disc16 <- as.data.table(disc16)
index16 <- grep("/2016", NCBOE$election_lbl)
NC16 <- NCBOE[index16]
tags <- NC16$ncid
st16 <- statewide[ncid%in%tags]
disc16 <- inner_join(NC16,st16)
disc16 <- as.data.table(disc16)

disc16[,VREP:=0]
disc16[,VDEM:=0]
disc16[,VUNA:=0]
disc16[,VLIB:=0]

disc16[,REP:=0]
disc16[,DEM:=0]
disc16[,UNA:=0]
disc16[,LIB:=0]

disc16[voted_party_cd=="REP",VREP:=1]
disc16[voted_party_cd=="DEM",VDEM:=1]
disc16[voted_party_cd=="UNA",VUNA:=1]
disc16[voted_party_cd=="LIB",VLIB:=1]


disc16[party_cd=="REP",REP:=1]
disc16[party_cd=="DEM",DEM:=1]
disc16[party_cd=="UNA",UNA:=1]
disc16[party_cd=="LIB",LIB:=1]

disc16 <- mutate(disc16, UVR = VREP+UNA, UVD = VDEM+UNA, UVL = VLIB + UNA)

disc16<- as.data.table(disc16)

disc16[UVR==1,UVR:=0]
disc16[UVD==1,UVD:=0]
disc16[UVL==1,UVL:=0]

disc16[UVR==2,UVR:=1]
disc16[UVD==2,UVD:=1]
disc16[UVD==2,UVL:=1]




