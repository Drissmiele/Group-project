#Date2_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#####################################
#select BUFF_DUST = 100
#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST =="100",] 
########################################

####dataframe for sampling date2
DATE2 <- Data1[Data1$Date == "2", ]

#calculate the aphids population growth at date2
logNaphids_D2 <- log(DATE2$aphid_live + 1) - log(DATE2$aphidsinoculated_init + 1) 
APG_D2 <- logNaphids_D2/20

#dataframe for seminatural habitat and aphids population growth at date1 
DATE2APG <- data.frame(DATE2, APG_D2)

####line graph (Figure 1B at date2)
#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
D2 <- ggplot(DATE2APG, aes(x = Pt.seminatural, y = APG_D2, color = Treatment))
D2 <- D2 + labs(x= "% seminatural habitat", y="Aphids population growth", title = "Effects of Treatment on LC and APG at Date 2")
#if scatter plot is needed
#q <- q + geom_point()
D2 <- D2 + geom_smooth(method = "lm")
print(D2)

