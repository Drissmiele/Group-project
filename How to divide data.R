library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
View(Data1)

<<<<<<< HEAD
install.packages("igraph")
library(igraph)

# graph depicting the relationship between variables
links <- data.frame(
  source=c("NE", "APG","NE", "NE", "PR","SF", "PR", "SF", "LC", "LC"),
  target=c("APG","FCB", "PR", "SF", "APG","APG", "FCB", "FCB", "PR", "SF")
)
network <- graph_from_data_frame(d=links, directed=T) 

# plot it
plot(network, vertex.color= "yellow", edge.arrow.size=1, vertex.label.font = 2, vertex.label.color="black")
legend(x =1, y =1, c("NE = Natural enemies", "PR = Parasitism rate", "SF = Syrfid fraction", "APG = Aphid population growth", "LC = Lanscape complexity", "FCB = Final Cabbage biomass"), cex = 1, title = "Relationships between variables")
=======

#remove variables that we don't need: plot_ID, Treatment, Field_Mgmt and crop type
Variables <- names(Data1)[c(-1, -11, -12, -13, -14, -15, -17)]
Data2 <- Data1[Variables]

#  created a new variable called plant_ID that is highly specific (plant number)
Data2$plant_ID <- paste(Data2$Treatment, Data2$Plant, sep = "_") 
unique(Data2$plant_ID) 

#  selecting  all the rows with buff = 100 in new dataframe (Data2)

Data2 <- Data2[Data2$BUFF_DIST == "100",] 

# separating Data2 into three data frames for each date

dframe_date1 <- data.frame(slice(Data2, 1:431))
dframe_date2 <- data.frame(slice(Data2, 432:863))
dframe_date3 <- data.frame(slice(Data2, 863:1271))
# => there is missing data

# checking for missing values with the complete function but fail (too much data)


>>>>>>> fec1a4f2ba4138550b168179ca8bd86dad64715e

