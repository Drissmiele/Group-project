#Date3_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#####################################
#select BUFF_DUST = 100
#selecting in Data 3 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST =="100",] 
########################################

###dataframe for sampling date3
DATE3 <- Data1[Data1$Date == "3", ]

#calculate the aphids population growth at date3
logNaphids_D3 <- log(DATE3$aphid_live + 1) - log(DATE3$aphidsinoculated_init + 1) 
APG_D3 <- logNaphids_D3/30

#dataframe for seminatural habitat and aphids population growth at date1 
DATE3APG <- data.frame(DATE3, APG_D3)

####line graph (Figure 1B at date3)
#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
D3 <- ggplot(DATE3APG, aes(x = Pt.seminatural, y = APG_D3, color = Treatment))
D3 <- D3 + labs(x= "% seminatural habitat", y="Aphids population growth", title = "Effects of Treatment on LC and APG at Date 3")
#if scatter plot is needed
#D3 <- D3 + geom_point()
D3 <- D3 + geom_smooth(method = "lm")
print(D3)
