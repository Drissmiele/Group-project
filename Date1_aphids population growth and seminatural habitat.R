#Date1_population growth and seminatural habitat
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

###dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]
summary(DATE1)
View(DATE1)

#calculate the aphids population growth at date1
logNaphids_D1 <- log(DATE1$aphid_live + 1) - log(DATE1$aphidsinoculated_init + 1) 
APG_D1 <- logNaphids_D1/10

<<<<<<< HEAD
#dataframe for seminatural habitat and aphids population growth at date1 
DATE1APG <- data.frame(DATE1,APG_D1)

=======
#data frame for seminatural habitat and aphids population growth at date1 
SA_D1 <- data.frame(DATE1$Pt.seminatural, APG_D1)
View(SA_D1)
>>>>>>> 13492120f2e8b6918406fd322069f6680f70c214


#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
q <- ggplot(DATE1APG, aes(x = Pt.seminatural...., y = APG_D1))
q <- q  + geom_quantile(size=1)
q <- q + labs(x= "% seminatural habitat", y="Aphids population growth")
q

#####reference
q <- ggplot(data2,aes(DAY,NUMBER,colour=COUNTRY))
q <- q + geom_line(size=1) 
q <- q + labs(x="日付", y="感染者数[人]", colour="国")
q <- q + scale_x_date(breaks = function(x) seq.Date(from=data2[1,1],to=max(x),by="4 days"),date_labels="%m月%d日")
q


#####################################
#lmer for plot (not yet, trying)
lmer(SA_D1$DATE1.Pt.seminatura~ SA_D1$APG_D1, data = SA_D1)

#Package that we use
#Aphid population growth was modelled using a linear mixed model
#install.packages("nlme")
library(nlme)
#install.packages("lme4")
library(lme4)