#Date1_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

###dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]
DATE1_700 <- subset(DATE1, DATE1$BUFF_DIST == "700")

###############line graph################
#calculate the aphids population growth at date1
logNaphids_D1_line <- log(DATE1_700$aphid_live + 1) - log(DATE1_700$aphidsinoculated_init + 1) 
APG_D1_line <- logNaphids_D1_line/10

#dataframe for seminatural habitat and aphids population growth at date1 
DATE1APG_line <- data.frame(DATE1_700,APG_D1_line)

#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
Line_APGD1 <- ggplot(DATE1APG_line, aes(x = Pt.seminatural, y = APG_D1_line, color = Treatment))
Line_APGD1 <- Line_APGD1 + labs(x = "% seminatural habitat", y = "Aphids population growth", title = "Effects of Treatment on LC and APG at Date 1")
# for scatter plot: use geom_point()
Line_APGD1 <- Line_APGD1 + geom_smooth(method = "lm")
Line_APGD1 <- Line_APGD1 +xlim(5,45) +ylim(-0.2 , 0.2)
print(Line_APGD1)

###############Bar graph################
#calculate the aphids population growth at date1
logNaphids_D1_bar <- log(DATE1$aphid_live+1) - log(DATE1$aphidsinoculated_init+1) 
APG_D1_bar <- logNaphids_D1_bar/10

#dataframe for seminatural habitat and aphids population growth at date1 
DATE1APG_bar <- data.frame(DATE1,APG_D1_bar)

#plot
Bar_APGD1 <- ggplot(DATE1APG_bar, aes(x = Treatment, y = APG_D1_bar, fill = Treatment))
Bar_APGD1 <- Bar_APGD1 + geom_bar(stat = "identity")
Bar_APGD1 <- Bar_APGD1 + labs(x = "Enemy exclution treatment", y = "APhids population growth", title = "Effects of Treatment on APG at Date 1")
Bar_APGD1 <- Bar_APGD1 + geom_hline(yintercept = 0, colour = "grey") 
#Bar_APGD1 <- Bar_APGD1 + ylim(-0.1 , 0.15)
plot(Bar_APGD1)

#add error bar
# DATE1APG_SE <- sd(DATE1APG$APG_D1)/ sqrt (length(DATE1APG$APG_D1))
# Bar_APGD1 <- Bar_APGD1 + geom_errorbar(aes(ymin = APG_D1 - DATE1APG_SE, ymax = APG_D1 + DATE1APG_SE))
