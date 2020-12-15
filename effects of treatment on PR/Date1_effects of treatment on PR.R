#Date1_parasitism rate

#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]
DATE1_200 <-  subset(DATE1, DATE1$BUFF_DIST == "200")

#########Line graph##############
# parasitism rate
PR_D1_line　<- DATE1_200$aphid_parasitized/(DATE1_200$aphid_live + DATE1_200$aphid_parasitized)

#dataframe including Parasitism rate at date1
DATE1PR_line <- data.frame(DATE1_200,PR_D1_line)

#line plot
library(ggplot2)
PR_D1_line <- ggplot(DATE1PR_line, aes(x = Pt.seminatural, y = PR_D1_line, color = Treatment))
PR_D1_line <- PR_D1_line + labs(x= "% seminatural habitat", y="Parasitism rate", title = "Effects of Treatment on PR at Date 1")
# for scatter plot: use geom_point()
PR_D1_line <- PR_D1_line + geom_smooth(method = "glm", method.args = list(family = binomial), se = F)
PR_D1_line <- PR_D1_line +xlim(5,45) +ylim(0, 1)
print(PR_D1_line)

#########Bar graph##############
# parasitism rate 
PR_D1_bar　<- DATE1$aphid_parasitized/(DATE1$aphid_live + DATE1$aphid_parasitized)

#add parasitism rate to the dataframe 
DATE1PR_bar <- data.frame(DATE1,PR_D1_bar)

#plot
library(ggplot2)
Bar_PRD1 <- ggplot(DATE1PR_bar, aes(x = Treatment, y = PR_D1_bar, fill = Treatment))
Bar_PRD1 <- Bar_PRD1 + geom_bar(stat = "identity")
Bar_PRD1 <- Bar_PRD1 + labs(x= "Enemy exclution treatment", y="Parasitism rate", title = "Effects of Treatment on PR at Date 1")
plot(Bar_PRD1)

