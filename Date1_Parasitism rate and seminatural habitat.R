#Date1_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#####################################
#If BUff_DUST is needed
#select BUFF_DUST = 100
#BUFF <- Data1[Data1$BUFF_DIST =="100",] 
########################################

###dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]
#DATE1_200_900 <- subset(DATE1, DATE1$BUFF_DIST == "200" | DATE1$BUFF_DIST =="900" )
DATE1_200 <-  subset(DATE1, DATE1$BUFF_DIST == "200")

#calculate the aphids population growth at date1
#logNaphids_D1 <- log(DATE1$aphid_live+1) - log(DATE1$aphidsinoculated_init+1) 
#APG_D1 <- logNaphids_D1/10

# parasitism rate - % seminatural habitat
PR_D1　<- DATE1_200$aphid_parasitized/(DATE1_200$aphid_live + DATE1_200$aphid_parasitized)

#dataframe for seminatural habitat and aphids population growth at date1 
DATE1PR <- data.frame(DATE1_200,PR_D1)

####line graph (Figure 1B at date1)
#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
PRD1 <- ggplot(DATE1PR, aes(x = Pt.seminatural, y = PR_D1, color = Treatment))
PRD1 <- PRD1 + labs(x= "% seminatural habitat", y="Parasitism rate", title = "Effects of Treatment on PR at Date 1")
# for scatter plot: use geom_point()
PRD1 <- PRD1 + geom_smooth(method = "glm", method.args = list(family = binomial), se = F)
PRD1 <- PRD1 +xlim(5,45) +ylim(0, 1)
print(PRD1)


#lme4
#library(lme4)
#glmer_PRD1 <- glmer(y ~ PR_D1, data = DATE1PR, family = "binomial"






# # trying to remove all NA
# Date_1a <- na.omit(Date_1)
# plot(Date_1a$parasitism_rate ~ Date_1a$Pt.seminatural)
# 
# #calculate the aphids parasitism rate (n = 1271)
# parasitism_rate　<- Data1$aphid_parasitized/(Data1$aphid_live+Data1$aphid_parasitized)
# Data1$parasitism_rate <- parasitism_rate
# 
# max(Date_1$Pt.seminatural, na.rm = TRUE)
# # max value of Pt.seminatural is 68%
# 
# p <- ggplot(Date_1, aes(x=Pt.seminatural, y=(parasitism_rate), color = Treatment) +
#          geom_point() +


