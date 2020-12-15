#Date2_parasitism rate
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

###dataframe for sampling date2 
DATE2 <- Data1[Data1$Date == "2", ]
DATE2_200 <-  subset(DATE2, DATE2$BUFF_DIST == "200")

#########Line graph##############
# parasitism rate 
PR_D2_line　<- DATE2_200$aphid_parasitized/(DATE2_200$aphid_live + DATE2_200$aphid_parasitized)

#dataframe including PR
DATE2PR_line <- data.frame(DATE2_200,PR_D2_line)

#plot
library(ggplot2)
PR_D2_line <- ggplot(DATE2PR_line, aes(x = Pt.seminatural, y = PR_D2_line, color = Treatment))
PR_D2_line <- PR_D2_line + labs(x= "% seminatural habitat", y="Parasitism rate", title = "Effects of Treatment on PR at Date 2")
# for scatter plot: use geom_point()
PR_D2_line <- PR_D2_line + geom_smooth(method = "glm", method.args = list(family = binomial), se = F)
PR_D2_line <- PR_D2_line +xlim(5,45) +ylim(0, 1)
print(PR_D2_line)

#########Bar graph##############
# parasitism rate 
PR_D2_bar　<- DATE2$aphid_parasitized/(DATE2$aphid_live + DATE2$aphid_parasitized)

#dataframe including PR
DATE2PR_bar <- data.frame(DATE2,PR_D2_bar)

#plot
library(ggplot2)
Bar_PRD2 <- ggplot(DATE2PR_bar, aes(x = Treatment, y = PR_D2_bar, fill = Treatment))
Bar_PRD2 <- Bar_PRD2 + geom_bar(stat = "identity")
Bar_PRD2 <- Bar_PRD2 + labs(x= "Enemy exclution treatment", y="Parasitism rate", title = "Effects of Treatment on PR at Date 2")
plot(Bar_PRD2)

