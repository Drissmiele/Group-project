####dataframe for sampling date2
DATE2 <- Data2[Data2$Date == "2", ]
summary(DATE2)
View(DATE2)

#calculate the aphids population growth at date2
logNaphids_D2 <- log(mean(DATE2$aphid_live) + 1) - log(mean(DATE2$aphidsinoculated_init) + 1) 
APG_D2 <- logNaphids_D2/10

#dataframe for seminatural habitat and aphids population growth at date1 
SA_D2 <- data.frame(DATE2$Pt.seminatura, APG_D2)
View(SA_D2)