#Date3_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

####dataframe for sampling date3
DATE3 <- Data1[Data1$Date == "3", ]
DATE3_700 <- subset(DATE3, DATE3$BUFF_DIST == "700")

###############line graph################
#calculate the aphids population growth at date1
logNaphids_D3_line <- log(DATE3_700$aphid_live + 1) - log(DATE3_700$aphidsinoculated_init + 1) 
APG_D3_line <- logNaphids_D3_line/30

#dataframe for seminatural habitat and aphids population growth at date1 
DATE3APG_line <- data.frame(DATE3_700,APG_D3_line)

#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
Line_APGD3 <- ggplot(DATE3APG_line, aes(x = Pt.seminatural, y = APG_D3_line, color = Treatment))
Line_APGD3 <- Line_APGD3 + labs(x = "% seminatural habitat", y = "Aphids population growth", title = "Effects of Treatment on LC and APG at Date 3")
# for scatter plot: use geom_point()
Line_APGD3 <- Line_APGD3 + geom_smooth(method = "lm")
Line_APGD3 <- Line_APGD3 + xlim(5,45) + ylim(-0.2 , 0.2)
print(Line_APGD3)

###############Bar graph######################
#calculate the aphids population growth at date1
logNaphids_D3_bar <- log(DATE3$aphid_live + 1) - log(DATE3$aphidsinoculated_init + 1) 
APG_D3_bar <- logNaphids_D3/30

#dataframe for seminatural habitat and aphids population growth at date1 
DATE3APG_bar <- data.frame(DATE3,APG_D3_bar)

#plot
Bar_APGD3 <- ggplot(DATE3APG_bar, aes(x = Treatment, y = APG_D3_bar, fill = Treatment))
Bar_APGD3 <- Bar_APGD3 + geom_bar(stat = "identity")
Bar_APGD3 <- Bar_APGD3 + labs(x = "Enemy exclution treatment", y = "APhids population growth", title = "Effects of Treatment on APG at Date 3")
Bar_APGD3 <- Bar_APGD3 + geom_hline(yintercept = 0, colour = "grey") 
plot(Bar_APGD3)


