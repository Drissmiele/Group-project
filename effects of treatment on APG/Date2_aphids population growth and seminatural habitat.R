#Date2_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

####dataframe for sampling date2
DATE2 <- Data1[Data1$Date == "2", ]
DATE2_700 <- subset(DATE2, DATE2$BUFF_DIST == "700")

###############line graph################
#calculate the aphids population growth at date1
logNaphids_D2_line <- log(DATE2_700$aphid_live+1) - log(DATE2_700$aphidsinoculated_init+1) 
APG_D2_line <- logNaphids_D2_line/20

#dataframe for seminatural habitat and aphids population growth at date1 
DATE2APG_line <- data.frame(DATE2_700,APG_D2_line)

#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
Line_APGD2 <- ggplot(DATE2APG_line, aes(x = Pt.seminatural, y = APG_D2_line, color = Treatment))
Line_APGD2 <- Line_APGD2 + labs(x= "% seminatural habitat", y="Aphids population growth", title = "Effects of Treatment on LC and APG at Date 2")
# for scatter plot: use geom_point()
Line_APGD2 <- Line_APGD2 + geom_smooth(method = "lm")
Line_APGD2 <- Line_APGD2 +xlim(5,45) +ylim(-0.2 , 0.2)
print(Line_APGD2)

###############Bar graph######################
#calculate the aphids population growth at date1
logNaphids_D2_bar <- log(DATE2$aphid_live+1) - log(DATE2$aphidsinoculated_init+1) 
APG_D2_bar <- logNaphids_D2_bar/20

#dataframe for seminatural habitat and aphids population growth at date1 
DATE2APG_bar <- data.frame(DATE2,APG_D2_bar)

#plot
Bar_APGD2 <- ggplot(DATE2APG_bar, aes(x = Treatment, y = APG_D2_bar, fill = Treatment))
Bar_APGD2 <- Bar_APGD2 + geom_bar(stat = "identity")
Bar_APGD2 <- Bar_APGD2 + labs(x= "Enemy exclution treatment", y="APhids population growth", title = "Effects of Treatment on APG at Date 2")
Bar_APGD2 <- Bar_APGD2 + geom_hline(yintercept=0, colour="grey") 
plot(Bar_APGD2)


