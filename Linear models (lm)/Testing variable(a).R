library(readxl)
library(igraph)

# dir()
links <- read_excel("Relationships between variables.xlsx")

# uncomment this line for first lines graph. Comment for complete graph
links <- links[1:20 ]

network <- graph_from_data_frame(d = links, directed = TRUE) 

# Now we plot it
plot(network, 
     edge.curved = 0.2,
     vertex.shape = "rectangle", 
     vertex.color = "white",
     vertex.frame.color = "white"
)

library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")


### Treatment - aphids density

lm1 <- lm(Data1$aphid_live ~ Data1$Treatment)
summary(lm1)
plot(Data1$aphid_live ~ as.numeric(as.factor(Data1$Treatment)), col = "red")


p <- ggplot(Data1, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)



# Treatment - aphids density - date 1/2/3

# DATE1

Date_1 <- Data1[Data1$Date == "1",]
lm1_1 <- lm(Date_1$aphid_live ~ Date_1$Treatment)
plot(Date_1$aphid_live ~ as.numeric(as.factor(Date_1$Treatment)))

# ggplot
p <- ggplot(Data_1, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# DATE2

Date_2 <- Data1[Data1$Date == "2",]
lm1_2 <- lm(Date_2$aphid_live ~ Date_2$Treatment)
plot(Date_2$aphid_live ~ as.numeric(as.factor(Date_2$Treatment)))

# ggplot

p <- ggplot(Date_2, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# DATE3

Date_3 <- Data1[Data1$Date == "3",]
lm1_3 <- lm(Date_3$aphid_live ~ Date_3$Treatment)
plot(Date_3$aphid_live ~ as.numeric(as.factor(Date_3$Treatment)))

#ggplot
library(ggplot2)
p <- ggplot(Date_3, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# conclusion: less and less aphids over time, effect of treatment is lower at date 3 than date 1..



### Treatment - aphid population growth (date1)

Date_1 <- Data1[Data1$Date == "1",]
logNaphids_all <- log(Date_1$aphid_live + 1) - log(Date_1$aphidsinoculated_init + 1) 
APG_date1 <- logNaphids_all/10
Date_1$APG_date1 <- APG_date1

lm1_4 <- lm(Date_1$APG_date1 ~ Date_1$Treatment)
summary(lm1_4)
p <- ggplot(Date_1, aes(x=Treatment,y=(APG_date1), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

### Treatment - aphid population growth (date2)

Date_2 <- Data1[Data1$Date == "2",]
logNaphids_all <- log(Date_2$aphid_live + 1) - log(Date_2$aphidsinoculated_init + 1) 
APG_date2 <- logNaphids_all/10
Date_2$APG_date2 <- APG_date2

lm1_5 <- lm(Date_2$APG_date2 ~ Date_2$Treatment)
summary(lm1_5)
p <- ggplot(Date_2, aes(x=Treatment,y=(APG_date2), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

### Treatment - aphid population growth (date3)


Date_3 <- Data1[Data1$Date == "3",]
logNaphids_all <- log(Date_3$aphid_live + 1) - log(Date_3$aphidsinoculated_init + 1) 
APG_date3 <- logNaphids_all/10
Date_3$APG_date3 <- APG_date3

lm1_6 <- lm(Date_3$APG_date3 ~ Date_3$Treatment)
summary(lm1_6)
p <- ggplot(Date_3, aes(x=Treatment,y=(APG_date3), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)


### Treatment - aphids + field management

# direct relationship

lm1 <- lm(Data1$aphid_live~Data1$Treatment)
summary(lm1)

# moderation effect with Field management
lm2 <- lm(Data1$aphid_live ~ Data1$Treatment + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$Treatment))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm2)

# model comparison
AIC(lm1, lm2)
# lm2 is better because lower AIC values indicate a better-fit model. 
# there is moderation 


            