#Date1_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#####################################
#If BUff_DUST is needed
#select BUFF_DUST = 100
#selecting in Data 2 all the rows with buff = 100
#BUFF <- Data1[Data1$BUFF_DIST =="100",] 
########################################

###dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]

#calculate the aphids population growth at date1
logNaphids_D1 <- log(DATE1$aphid_live+1) - log(DATE1$aphidsinoculated_init+1) 
APG_D1 <- logNaphids_D1/10

#dataframe for seminatural habitat and aphids population growth at date1 
DATE1APG <- data.frame(DATE1,APG_D1)

####line graph (Figure 1B at date1)
#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
D1 <- ggplot(DATE1APG, aes(x = Pt.seminatural, y = APG_D1, color = Treatment))
D1 <- D1 + labs(x= "% seminatural habitat", y="Aphids population growth", title = "Effects of Treatment on LC and APG at Date 1")
#if scatter plot is needed
#D1 <- D1 + geom_point()
D1 <- D1 + geom_smooth(method = "lm")
print(D1)