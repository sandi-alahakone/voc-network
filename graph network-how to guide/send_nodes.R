
# References --------------------------------------------------------------

### https://kateto.net/network-visualization
### https://rpubs.com/simaan84/410145

# Set working drive -------------------------------------------------------

setwd("C:/Users/149366/OneDrive/r/database/network vis") ## Uni computer
setwd("C:/Users/sanda/OneDrive/r/database/network vis")  ## laptop

# Install packages --------------------------------------------------------

install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("ggraph")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")
install.packages("dplyr")
devtools::install_github('ramnathv/htmlwidgets')
install.packages("htmlwidgets")
install.packages("shiny")

# Load packages -----------------------------------------------------------

library(igraph)
library(network)
library(sna)
library(ggraph)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
library(dplyr)
library(htmlwidgets)
library(shiny)

# Load node and edge data -------------------------------------------------

nodes <- read.csv("volatilestudies_nodes.csv")
links <- read.csv("volatilestudies_edges.csv")

head(nodes)
head(links)

# Create an igraph object -------------------------------------------------

net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
net

# nodes and edges attributes
E(net)                 # The edges of the "net" object
V(net)                 # The vertices of the "net" object
E(net)$edgetype.label  # Edge attribute "type"
V(net)$node.label      # Vertex attribute "media"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[node.label=="(methyldisulfanyl)methane"]
E(net)[edgetype.label=="sandilong -> sulphur"]

# You can also examine the network matrix directly:
net[3,]
net[3,7]

# Get an edge list or a matrix:
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="edge.type")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")


# Create plot -------------------------------------------------------------

plot(net)

# Remove loops in the graph
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# Reduce arrow size and remove labels
plot(net, edge.arrow.size=.1,vertex.label=NA)

# Change other plot parameters
?igraph.plotting
# different layouts
l <- layout_on_sphere(net)
l <- layout_with_fr(net, dim=3)
l <- layout_with_kk(net)
l <- layout_with_graphopt(net, charge=0.02, spring.length = 1000)
l <- layout_with_lgl(net)
l <- layout_with_mds(net)
##
plot(net, edge.arrow.size=.1, vertex.color= V(net)$node.colour, vertex.label=V(net)$visible.label)

# Add attributes to igraph object -----------------------------------------

V(net)$color <- V(net)$node.colour
V(net)$size <- V(net)$node.size
#V(net)$size <- 5
V(net)$label <- V(net)$visible.label
E(net)$edge.color <- '#9F95A2'
E(net)$width <- 1

graph_attr(net, "layout") <- layout_with_lgl

plot(net, layout=l)

# Interactive plots -------------------------------------------------------
tkid <- tkplot(net, edge.arrow.size=.1,
               vertex.label=NA, vertex.color= V(net)$node.colour,  
               vertex.size= V(net)$node.size, vertex.label=V(net)$visible.label,
               layout=l) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=l)

# Interactive JS visualization with visNetwork ----------------------------

library('visNetwork') 

nodes <- read.csv("volatilestudies_nodes.csv")
links <- read.csv("volatilestudies_edges.csv")

save(nodes, file = "nodesVOC.Rdata")
save(links, file = "linksVOC.Rdata")

head(nodes)
head(links)

?mutate
# Check available options
?visNodes
?visEdges
?visInteraction
?visNetwork
?visPhysics

## DOES NOT WORK :( ----

visNetwork(nodes, links,   background="#eeefff",
           main="Network", submain="And what a great network it is!",
           footer= "Hyperlinks and mentions among media sources")

vis.nodes <- nodes
vis.links <- links

vis.nodes$id <- vis.nodes$id
vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- FALSE # Nodes will NOT drop shadow
vis.nodes$title  <- vis.nodes$node.label # Text on click
vis.nodes$label  <- vis.nodes$visible.label # Node label
vis.nodes$size   <- vis.nodes$node.size # Node size
vis.nodes$borderWidth <- 0.5 # Node border width
vis.nodes$color.background <- vis.nodes$node.colour
vis.nodes$color.border <- vis.nodes$node.colour
test <-visNetwork(vis.nodes, vis.links, height = "650px", width = "100%", background="#292630",
                  main="Volatile Organic Compounds", submain="detected from various studies")

test

visNetwork(nodes, links)


test2 <- visNetwork(vis.nodes, vis.links)
test2
##
visNetwork(nodes, links) %>%
  visNodes(id= nodes$id,
           title = nodes$node.label,
           label = nodes$visible.label,
           size = nodes$node.size,
           color = list(background= nodes$node.colour, border= nodes$node.colour))


## This one works! :) ----
visNetworkNodes <- data.frame(nodes) %>%
  mutate(id= nodes$node.label,
         label = visible.label,
         title = node.label,
         font.color = node.colour,
         font.size = node.size,
         size = node.size,
         color = list(background= node.colour, border= node.colour),
         physics = TRUE)

visNetworkLinks <- data.frame(from= links$from.node.label, 
                              to = links$to.node.label, 
                              width = 1,
                              color = list(color = '#464549', highlight= "#E0DDEA"),
                              smooth = FALSE,
                              physics = TRUE)

voc_net <- visNetwork(nodes = visNetworkNodes,
                  edges = visNetworkLinks, background="#292630")

voc_net

voc_net2<- visPhysics(graph = voc_net, solver = "forceAtlas2Based",
           forceAtlas2Based = list(gravitationalConstant = -300 ))

voc_net3 <- visPhysics(graph = voc_net2, enabled = FALSE)

voc_net2
blah1<- voc_net$x
blah2 <- blah1$nodes

visPhysics(graph = voc_net, enabled = FALSE,
           barnesHut = list(gravitationalConstant = -5000), stabilization = TRUE)


testvocnet <-     visNetwork(nodes = visNetworkNodes,
                             edges = visNetworkLinks, background="#292630")%>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -100 ))

testvocnet
###

yikes <-visIgraph(net)
yikes

visYikes <- toVisNetworkData(net)

nodeYikes <- visYikes$nodes
edgeYikes <- visYikes$edges

visNetwork(nodeYikes, edgeYikes)
# Interactive JS visualization with threejs -------------------------------

net.js <- net
graph_attr(net.js, "layout") <- NULL


?graphjs


gjs <- graphjs(net.js, 
               main = "VOC Network!",
               bg = "black",
               showLabels = T,
               stroke = F,
               edge.width = 5,
               edge.color = '#9F95A2',
               layout = layout_with_fr(net.js, dim = 3))


print(gjs)
saveWidget(gjs, file = "VOC-Network-gjs.html")
browseURL("VOC-Network-gjs.html")

# Interactive JS visualization with networkD3 -----------------------------

library(networkD3)

nodes <- read.csv("volatilestudies_nodes.csv")
links <- read.csv("volatilestudies_edges.csv")

links.d3 <- data.frame(from=as.numeric(links$from.num.id)-1,
                       to=as.numeric(links$to.num.id)-1)

#save(links.d3, file = "voclinks.Rdata")

nodes.d3 <- cbind(idn=factor(nodes$node.label, levels = nodes$node.label), nodes)

#save(nodes.d3, file = "vocnodes.Rdata")

YourColors <- 'd3.scaleOrdinal()
                  .domain(["decomp", "human", "slongi", "Alcohol", "Aldehyde", "Alkane", "Alkene", "Aromatic", "Carboxylic acid", "Cycloalkane", "Cycloalkene", "Ester", "Ether", "Halogenated", "Ketone", "Monoterpene", "Nitrogen containing", "Sulphur containing", "Terpene" ])
                  .range(["#1D3861", "#305094", "#606FB3", "#F8766D", "#F2B35A", "#C99800", "#A3A500", "#6BB100", "#497B58", "#00BF7D", "#00C0AF", "#00BCD8", "#00B0F6", "#619CFF", "#D6B6FF", "#A378DB", "#E76BF3", "#C884B5", "#FF67A4"])'

forceNetwork(Links = links.d3, Nodes = nodes.d3, Source="from", Target="to",
             NodeID = "idn", Group = "nodetype.label", colourScale = JS(YourColors),linkWidth = 1,
             linkColour = "#afafaf", fontSize=20, zoom=T, legend=F,
             Nodesize=7, opacity = 0.9, charge=-300)


#####test junkyard

testnod <- data.frame(id = 1:3, group = c("B", "A", "B"))

nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
edges <- data.frame(from = c(1,2), to = c(2,3))

visNetwork(nodes, edges) %>%
  visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
  visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
  addFontAwesome()
