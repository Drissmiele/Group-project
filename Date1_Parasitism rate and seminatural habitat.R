Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
Date_1 <- Data1[Data1$Date == "1", ]

# parasitism rate - % seminatural habitat
parasitism_rate　<- Date_1$aphid_parasitized/((Date_1$aphid_live)+(Date_1$aphid_parasitized))
Date_1$parasitism_rate <- parasitism_rate

Date_1a <- na.omit(Date_1)
plot(Date_1a$parasitism_rate ~ Date_1a$Pt.seminatural)

<<<<<<< HEAD
#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- Data1$aphid_parasitized/(Data1$aphid_live+Data1$aphid_parasitized)
Data1$parasitism_rate <- parasitism_rate

=======
max(Date_1$Pt.seminatural, na.rm = TRUE)
# max value of Pt.seminatural is 68%



plot(Date_1$parasitism_rate ~ Date_1$Pt.seminatural)
library(ggplot2)
D1 <- ggplot(Date_1, aes(x = Pt.seminatural, y = (parasitism_rate), color = Treatment)) 
print(D1)
D1 <- D1 + labs(x= "% seminatural habitat", y="parasitism_rate", title = "Effects of Treatment on LC and APG at Date 1")
D1 <- D1 + geom_smooth(method = "lm")
D1 <- D1 +xlim(5,45) +ylim(-0.2 , 0.2)
print(D1)

D2 <- Date_1[Date_1$parasitism_rate < 0, ]
unique(D2$parasitism_rate)


p <- ggplot(Date_1, aes(x=Pt.seminatural, y=(parasitism_rate), color = Treatment) +
         geom_point() +
         geom_smooth(method = "lm")
print(p)     


p <- ggplot(Data1, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

unique(parasitism_rate)
D <- Date_1[Date_1$Treatment == "H",]
unique(D$parasitism_rate)
#plot
library(ggplot2)
D1 <- ggplot(Date_1, aes(x=Pt.seminatural,y=(parasitism_rate), color= Treatment)) +
               geom_smooth(method = "lm")
D1 <- D1 +xlim(5,45) +ylim(0 , 1)


print(D1)

p <- ggplot(Data1, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)


print(D1)
labs(x= "% seminatural habitat", y="Aphids population growth", title = "Effects of Treatment on LC and APG at Date 1")
>>>>>>> 6e82488a3cb7ef18915e9a4983141d31d2bcc659
