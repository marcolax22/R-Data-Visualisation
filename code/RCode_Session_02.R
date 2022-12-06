title: "Data Visualization with R - Session 2/2"
authors: "Leslie Fischer"
date: "11.11.2022"

########################
## 1. Preparation     ##
########################

# clear the workspace
rm(list = ls()) 

# set the working directory to the folder containing the workshop files:
setwd("~/Desktop/Workshop")

# install packages
x <- c("readxl", "sf", "igraph")

# load the required packages 
lapply(x, library, character.only = TRUE) 

#####################
## 2. Spatial Data ##
#####################

# load the data
library(readxl)
nodes <- read_excel("NODES.xlsx")
links <- read_excel("EDGES.xlsx")

# plot as pie
pie(nodes$visits, labels=nodes$country, col = rainbow(length(nodes$visits)), main= "Visits")

# enable dataset
library(sf)
capital <- st_as_sf(nodes, coords = c("longitude_cap", "latitude_cap"), crs = 4326)

# have a look at data
print(capital, n=4)

# plot data points
plot(st_geometry(capital))

# load spatial data set for Europe
europe <- st_read(file.path("NUTS_RG_60M_2021_4326"), crs=4326)

# plot map 
plot(st_geometry(europe), col="lightgrey")

# add data points
plot(st_geometry(capital), pch=10, col = "blue", add=T)

# too large: subset only the countries we are interested in: DE, IT, SE, PL
better <- europe[europe$CNTR_CODE=="DE" | europe$CNTR_CODE=="IT" |
                 europe$CNTR_CODE=="PL"| europe$CNTR_CODE=="SE",]

plot(st_geometry(better), col="lightgrey")
plot(st_geometry(capital), pch=16, col = "red", add=T)

# merge data sets 
better<- merge(better, nodes[,c("visits", "CNTR_CODE")], by = "CNTR_CODE", all = TRUE) 

# how many features are in each category or level?
summary(better$visits)

# convert to factor
better$visits <- as.factor(better$visits)

# check
class(better$visits)

# count the number of unique values or levels
length(levels(better$visits))

# create a color palette of 4 colors - one for each factor level
road_palette <- c("grey80", "grey60", "grey40", "grey20" )
road_palette

# create a vector of colors - one for each feature in the vector object
# according to its attribute value
all_road_colors <- c("grey80", "grey60", "grey40", "grey20")[better$visits]
head(all_road_colors)

# plot the lines data, apply a different color to each factor level
plot(st_geometry(better), col= all_road_colors)
plot(st_geometry(capital), pch=16, col = "red", add=T)


# add a legend to the map
legend(x = "right", # location of legend
       c("  6 visits", "10 visits", "13 visits", "19 visits" ), 
       cex = .5, # adjust font size
       fill = road_palette) # color palette to use to fill objects in legend.

# you can have more fun with the colors, just keep in mind that variables can be different scaled
fun_palette <- c("pink", "blue", "yellow", "green" )
fun <- c("pink", "blue", "yellow", "green")[better$visits]
plot(st_geometry(better), col= fun)
plot(st_geometry(capital), pch=16, col = "red", add=T)

# add a legend to the map
legend(x = "right", # location of legend
       c("  6 visits", "10 visits", "13 visits", "19 visits" ), 
       cex = .5, # adjust font size
       fill = fun_palette) # color palette to use to fill objects in legend.

#####################
## 3. Network Data ##
#####################

# create a network (network part is adapted from: https://kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf)
library(igraph)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

# plot network
plot(net, vertex.color="lightgrey", vertex.label=V(net)$id, 
     vertex.label.font=2, vertex.label.color="black", vertex.size=40,
     vertex.label.cex=.7, edge.color="gray85")

# set node (/vertix) size based on number of visits:
V(net)$size <- V(net)$visits*4

# number of visits  in the network dataset: take out vertex.size=40,
plot(net, edge.arrow.size=.3, vertex.color="lightgrey", vertex.label=V(net)$id, 
     vertex.label.font=2, vertex.label.color="black", 
     vertex.label.cex=.7, edge.color="gray85")

# color by aerial distance
colrs <- E(net)$color <- ifelse( E(net)$dummy == "1", "tomato", "gold")

# plot
plot(net, edge.arrow.size=.5, vertex.color="lightgrey", vertex.label=V(net)$id, 
     vertex.label.font=2, vertex.label.color="gray40", 
     vertex.label.cex=.7)

# add legend
legend(x=-1.1, y=-1.5, c("Long-distance"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)
