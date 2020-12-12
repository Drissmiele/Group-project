library(readxl)
library(igraph)

# dir()
links <- read_excel("Relationships between variables.xlsx")

# uncomment this line for first lines graph. Comment for complete graph
links <- links[1:20 ]

network <- graph_from_data_frame(d = links, directed = TRUE) 

# plot it
plot(network, 
     edge.curved = 0.2,
     vertex.shape = "rectangle", 
     vertex.color = "white",
     vertex.frame.color = "white"
)

library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
View(Data1)
# Treatment - aphids density

lm1 <- lm(Data1$aphid_live ~ Data1$Treatment)
summary(lm1)
plot(Data1$aphid_live ~ Data1$Treatment)
# the effect of treatment on aphids density is significant for all treatments.

#6 aphids and biomass
lm6 <- lm(Data1$aphid_live ~ Data1$Biomass_fin)
lm6 <- lm(Data1$Biomass_fin ~ Data1$aphid_live)
summary(lm6)  
remove(lm6_1)
plot(Data1$Biomass_fin ~ Data1$aphid_live, ylab = "Biomass", xlab = "APhids density")
#P value = 0.0523

#8 Management and syruphids
lm8 <- lm(Data1$syrphidl_p ~ Data1$Field_Mgmt)
summary(lm8)
plot(Data1$syrphidl_p ~ as.factor(Data1$Field_Mgmt), ylab = "Syrphid", xlab = "Field Management")
# p value =  <2e-16

#10Crop_mat	and aphids
lm10 <- lm(Data1$aphid_live ~ Data1$cropmaturity_init)
summary(lm10)
plot(Data1$aphid_live ~ Data1$cropmaturity_init, ylab = "Aphids density", xlab = "crop maturity")
#p value = <2e-16, ***

#12Crop_type and	aphids
lm12 <- lm(Data1$aphid_live ~ Data1$croptype)
summary(lm12)
plot(Data1$aphid_live ~ Data1$croptype, ylab = "Aphids density", xlab ="Crop type" )
#p value  = 3.65e-10, ***

#14 Seminat	and aphids
lm14 <- lm(Data1$aphid_live ~ Data1$Pt.seminatural)
summary(lm14)
plot(Data1$aphid_live ~ Data1$Pt.seminatural, ylab = "Aphids density", xlab = "Seminatural habitat")
# p value <2e-16 ***

#16 Seminat	and aphids_parasitized
lm16 <- lm(Data1$aphid_parasitized ~ Data1$Pt.seminatural)
summary(lm16)
plot(Data1$aphid_parasitized ~ Data1$Pt.seminatural,ylab = "Aphids parasitized", xlab = "Seminatural habitat")
#p value < 2e-16 ***

#18 Management and Biomass
lm18 <- lm(Data1$Biomass_fin ~ Data1$Field_Mgmt)
summary(lm18)
plot(Data1$Biomass_fin ~ as.factor(Data1$Field_Mgmt), ylab = "Biomass", xlab = "Field management")
#p value  <2e-16 ***

#20 aphids and Date
lm20 <- lm(Data1$aphid_live ~ Data1$Date)
summary(lm20)
plot(Data1$aphid_live ~ Data1$Date, ylab = "Aphids", xlab = "Date", col="red")
#p value  <2e-16 ***


ANOVAlm1 <- aov(Data1$aphid_live ~ Data1$Treatment, data = Data1)
summary(ANOVAlm1)
library("agricolae")
HSDtest <- HSD.test(ANOVAlm1, "Treatment" )
HSDtest
boxplot(Data1$aphid_live ~ Data1$Treatment, data = Data1)

