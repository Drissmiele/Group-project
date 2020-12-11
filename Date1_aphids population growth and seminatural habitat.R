###dataframe for sampling date1 
DATE1 <- Data2[Data2$Date == "1", ]
summary(DATE1)
View(DATE1)

#calculate the aphids population growth at date1
logNaphids_D1 <- log(DATE1$aphid_live + 1) - log(DATE1$aphidsinoculated_init + 1) 
APG_D1 <- logNaphids_D1/10

#data frame for seminatural habitat and aphids population growth at date1 
SA_D1 <- data.frame(DATE1$Pt.seminatural, APG_D1)
View(SA_D1)


#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
ggplot(SA_D1, 
       aes(x = SA_D1$DATE1.Pt.seminatura, y = SA_D1$APG_D1)) +
  geom_smooth() 


#lmer for plot (not yet, trying)
lmer(SA_D1$DATE1.Pt.seminatura~ SA_D1$APG_D1, data = SA_D1)


#Package that we use
#Aphid population growth was modelled using a linear mixed model
#install.packages("nlme")
library(nlme)
#install.packages("lme4")
library(lme4)