Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
Date_1 <- Data1[Data1$Date == "1", ]

# parasitism rate - % seminatural habitat
parasitism_rate　<- Date_1$aphid_parasitized/((Date_1$aphid_live)+(Date_1$aphid_parasitized))
Date_1$parasitism_rate <- parasitism_rate

# trying to remove all NA
Date_1a <- na.omit(Date_1)
plot(Date_1a$parasitism_rate ~ Date_1a$Pt.seminatural)


#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- Data1$aphid_parasitized/(Data1$aphid_live+Data1$aphid_parasitized)
Data1$parasitism_rate <- parasitism_rate

max(Date_1$Pt.seminatural, na.rm = TRUE)
# max value of Pt.seminatural is 68%

p <- ggplot(Date_1, aes(x=Pt.seminatural, y=(parasitism_rate), color = Treatment) +
         geom_point() +


