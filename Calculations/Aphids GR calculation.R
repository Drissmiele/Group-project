#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_1 <- Data1[Data1$Date == "1", ]

#calculate the aphids population growth for Date 1
logNaphids_all <- log(Date_1$aphid_live + 1) - log(Date_1$aphidsinoculated_init + 1) 
APG_D1 <- logNaphids_all/10

#make new data frame which include aphids population growth rate for date 1
Date_1$APG_D1 <- APG_D1
