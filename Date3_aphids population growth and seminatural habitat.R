###dataframe for sampling date3
DATE3 <- Data2[Data2$Date == "3", ]
summary(DATE3)
View(DATE3)

#calculate the aphids population growth at date3
logNaphids_D3 <- log(mean(DATE3$aphid_live)+1) - log(mean(DATE3$aphidsinoculated_init)+1) 
APG_D3 <- logNaphids_D3/10

#dataframe for seminatural habitat and aphids population growth at date1 
SA_D3 <- data.frame(DATE3$Pt.seminatura, APG_D3)
View(SA_D3)
