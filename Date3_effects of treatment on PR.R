#Date3_Parasitism rate
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

###dataframe for sampling date3 
DATE3 <- Data1[Data1$Date == "3", ]
DATE3_200 <-  subset(DATE3, DATE3$BUFF_DIST == "200")

#########Line graph##############
# parasitism rate 
PR_D3_line　<- DATE3_200$aphid_parasitized/(DATE3_200$aphid_live + DATE3_200$aphid_parasitized)

#dataframe including Parasitism Rate 
DATE3PR_line <- data.frame(DATE3_200,PR_D3_line)

#plot for % Seminatural habitat and Parasitism rate
library(ggplot2)
PR_D3_line <- ggplot(DATE3PR_line, aes(x = Pt.seminatural, y = PR_D3_line, color = Treatment))
PR_D3_line <- PR_D3_line + labs(x= "% seminatural habitat", y="Parasitism rate", title = "Effects of Treatment on PR at Date 3")
# for scatter plot: use geom_point()
PR_D3_line <- PR_D3_line + geom_smooth(method = "glm", method.args = list(family = binomial), se = F)
PR_D3_line <- PR_D3_line +xlim(5,45) +ylim(0, 1)
print(PR_D3_line)

#########Bar graph##############
# parasitism rate - Treatment
PR_D3_bar　<- DATE3$aphid_parasitized/(DATE3$aphid_live + DATE3$aphid_parasitized)

#dataframe including parasitism rate at date3 
DATE3PR_bar <- data.frame(DATE3,PR_D3_bar)

#plot 
library(ggplot2)
Bar_PRD3 <- ggplot(DATE3PR_bar, aes(x = Treatment, y = PR_D3_bar, fill = Treatment))
Bar_PRD3 <- Bar_PRD3 + geom_bar(stat = "identity")
Bar_PRD3 <- Bar_PRD3 + labs(x= "Enemy exclution treatment", y="Parasitism rate", title = "Effects of Treatment on PR at Date 3")
plot(Bar_PRD3)

