# Build interactive web application with Shiny --------------------------------------------
library(shiny)
library(networkD3)
library(visNetwork)
library(dplyr)
library(rsconnect)


load(file = "data/linksVOC.Rdata")
load(file = "data/nodesVOC.Rdata")


nodes <- read.csv('volatilestudies_nodes.csv', stringsAsFactors=FALSE, sep=",", header=TRUE)
links <- read.csv('volatilestudies_edges.csv', stringsAsFactors=FALSE, sep=",", header=TRUE)

## Build a user interface ----
#Define UI
ui <- fluidPage(
  titlePanel(h1("Interactive Volatile Organic Compound Network")),
  
  
  sidebarLayout(position = "right",
    mainPanel("VOC Network", visNetworkOutput("vocnetwork", height = "1000px")),
    sidebarPanel(h2("Welcome"),
                 br(),
                 br(),
                 
                 h6("created by sandi :)"))
  )
  

)

#Define server logic
server <- function(input, output){
  
  output$vocnetwork <- renderVisNetwork({
    #load data
    #nodes <- load(file = "app-1/data/nodesVOC.Rdata")
    #links <- load(file = "app-1/data/linksVOC.Rdata")
    
    #edit nodes and edges
    visNetworkNodes <- data.frame(nodes) %>%
      mutate(id= nodes$node.label,
             label = visible.label,
             title = node.label,
             font.color = nodes$node.colour,
             font.size = node.size,
             size = node.size,
             color.background = nodes$node.colour,
             color.border = nodes$node.colour,
             physics = TRUE)
    
    visNetworkLinks <- data.frame(from= links$from.node.label, 
                                  to = links$to.node.label, 
                                  width = 1,
                                  color = list(color = '#464549', highlight= "#E0DDEA"),
                                  smooth = FALSE,
                                  physics = TRUE)
    #Build network
    visNetwork(nodes = visNetworkNodes,
               edges = visNetworkLinks, background="#1C1A23")%>%
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -100 ))
    
    
    #forceNetwork(Links = links.d3, Nodes = nodes.d3, Source="from", Target="to",
                 #NodeID = "idn", Group = "nodetype.label", colourScale = JS(YourColors),
                 #linkColour = "#afafaf", fontSize=20, zoom=T, legend=F,
                 #Nodesize=7, opacity = 1, charge=-300, width = "700px", height = "1400px")
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)

shiny::runApp()

rsconnect::setAccountInfo(name='salahakone',
                          token='D01625F169011596269EAF79EBF28A0F',
                          secret='OB8CyB2IJi7cDOQCpITyC68+3JfWeSDy637ETd1q')


rsconnect::deployApp('C:/Users/149366/OneDrive/r/database/network vis/app-1/',appFiles = c('app.r', 'volatilestudies_nodes.csv','volatilestudies_edges.csv'),
                     account = 'salahakone', server = 'shinyapps.io')