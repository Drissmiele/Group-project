#Date1_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

###dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]

#calculate the aphids population growth at date1
logNaphids_D1 <- log(DATE1$aphid_live+1) - log(DATE1$aphidsinoculated_init+1) 
APG_D1 <- logNaphids_D1/10

#dataframe for seminatural habitat and aphids population growth at date1 
DATE1APG <- data.frame(DATE1, APG_D1)

####line graph (Figure 1B at date1)
#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
q <- ggplot(DATE1APG, aes(x = Pt.seminatural, y = APG_D1, color = Treatment))
q <- q + labs(x= "% seminatural habitat", y="Aphids population growth", title = "Effects of Treatment on LC and APG at Date 1")
#if scatter plot is needed
#q <- q + geom_point()
q <- q + geom_smooth(method = "lm")
print(q)
