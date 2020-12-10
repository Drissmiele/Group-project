library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
View(Data1)

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

