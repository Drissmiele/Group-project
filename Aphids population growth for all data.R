#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#calculate the aphids population growth at date1

logNaphids_all <- log(Data1$aphid_live+1) - log(Data1$aphidsinoculated_init+1) 
APG_all <- logNaphids_all/10

APG_all
View(APG_all)

