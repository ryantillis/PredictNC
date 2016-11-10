


#modelset only goes through modelset 1:998

aggD[, Democrats := 0]
aggD[, Republicans := 0]
aggD[, Libertarians := 0]
aggD[, Unaffiliated := 0]
aggD[, White := 0]
aggD[, Black := 0]
#DONT FORGET ADDED UNDERSCORE
aggD[, American_Indian := 0]
aggD[, Other := 0]
aggD[, Hispanic := 0]
aggD[, Male := 0]
aggD[, Female := 0]
aggD[, Total := 0]

#write.csv(aggD,"aggwithrowsums.csv")

modelset <- aggD[row_sums > 222]

aggregate_date <- dat %>% group_by(county_desc, election_lbl) %>% summarise_each(funs(sum), DEM, REP, UNA, LIB)