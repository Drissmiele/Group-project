#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

#calculate the aphids population growth 
logNaphids_all <- log(Data1$aphid_live + 1) - log(Data1$aphidsinoculated_init + 1) 
APG_all <- logNaphids_all/10

#view aphids population growth for 12000 data
View(APG_all)

#make new data frame which include aphids population growth rate
Data1_APG_all <- data.frame(Data1, APG_all)
View(Data1_APG_all)
