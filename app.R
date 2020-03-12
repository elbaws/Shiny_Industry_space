rm(list = ls(all = TRUE))

library(shiny)
library(ggplot2)
library(ggnetwork)
library(network)
library(reshape2)
library(ggrepel)


#####################################
#######Data##########################
#####################################

setwd("")

load("region_networks.RData")
load("nuts_labels.RData")
load("nace4_labels.RData")



#####################################
#######Functions and variables#######
#####################################

region.list <- paste0(toupper(nuts.labels$region), " - ", nuts.labels$nuts.label)

spec.change <- function(region, year1, year2) {
  ggnet.x <- get(paste0("ggnet.", region))
  ggnet.x$lq.change <- NA
  ggnet.x$lq.change[ggnet.x[[paste0("lq", year1)]] == 1 & ggnet.x[[paste0("lq", year2)]] == 0] <- 1 #Lost
  ggnet.x$lq.change[ggnet.x[[paste0("lq", year1)]] == 0 & ggnet.x[[paste0("lq", year2)]] == 0] <- 2 #Never had
  ggnet.x$lq.change[ggnet.x[[paste0("lq", year1)]] == 0 & ggnet.x[[paste0("lq", year2)]] == 1] <- 3 #Gained
  ggnet.x$lq.change[ggnet.x[[paste0("lq", year1)]] == 1 & ggnet.x[[paste0("lq", year2)]] == 1] <- 4 #Kept
  return(ggnet.x$lq.change)
}


plot.network <- function(type, region, year1, year2) {
  ggnet.x <- get(paste0("ggnet.", region))
  ggnet.x$lq.change <- spec.change(region, year1, year2)
  ggnet.x$lq1.x <- ggnet.x[[paste0("lq", year1)]]
  ggnet.x$densitydum <- ggnet.x[[paste0("densitydum", year2)]]
  y1incl <- ifelse(type == 1, list(2:3), list(1:2))[[1]]

  p <- ggplot(ggnet.x, aes(x, y, xend = xend, yend = yend)) +
    geom_edges(colour = "black", size = .05) +
    geom_nodes(data = ggnet.x[ggnet.x$lq.change %in% y1incl,], aes(size = log10(emp2011n+1)), fill = "grey75", colour = "white", alpha = .8, stroke = .5, shape = 21)

  if (type == 1) {
    p <- p + geom_nodes(data = ggnet.x[ggnet.x$lq1.x == 1,], aes(size = log10(emp2011n+1), fill = factor(lq1.x)), colour = "white", stroke = .5, shape = 21) +
      scale_fill_manual(name = paste("Specialised in", year1), labels = c("No", "Yes"), values = c("grey75", "royalblue"), limits = c("0", "1"),
                        guide = guide_legend(override.aes = list(size = 6, stroke = 0),
                                             title.position = "top", label.vjust = 1.5, title.vjust = 0,
                                             direction = "horizontal", label.position = "bottom"))
    
  } else if (type == 2 | type == 3) {
    p <- p + geom_nodes(data = ggnet.x[ggnet.x$lq.change %in% 3:4,], aes(size = log10(emp2011n+1), fill = factor(lq.change)), colour = "white", alpha = 1, stroke = .5, shape = 21) +
      scale_fill_manual(name = paste0("Change ", year1, "-", year2), labels = c("Kept", "Gained"), values = c("royalblue", "green"), limits = c("4", "3"),
                        guide = guide_legend(override.aes = list(size = 6, stroke = 0),
                                             title.position = "top", label.vjust = 1.5, title.vjust = 0,
                                             direction = "horizontal", label.position = "bottom"))
  }
  
  if (type == 3) {
    p <- p + geom_nodes(data = ggnet.x[ggnet.x$lq.change == 3 & ggnet.x$densitydum != 0, ], aes(size = log10(emp2011n+1.2), colour = factor(densitydum)), stroke = 2.5, shape = 1) +
      scale_colour_manual(name = "Density of new", values = c("#F8766D", "#7CAE00"), labels = c("Low", "High"), limits = c(1, 2),
                          guide = guide_legend(override.aes = list(size = 5),
                                               title.position = "top", label.vjust = 1.5, title.vjust = 1,
                                               direction = "horizontal", label.position = "bottom"))
  }

  p <- p + scale_size(range = c(3, 10), guide = F) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = c(.1, .86), legend.text = element_text(size = 13), legend.key = element_blank(),
          legend.title = element_text(size = 13, face = "bold"), legend.spacing.y = unit("2", "mm"),
          panel.background = element_blank(), text = element_text(family = "Myriad Pro"))

  return(p)
}

industry.label <- function(nace) {
  return(nace4.labels$nace.4d.label[nace4.labels$nace == nace])
}



#####################################
##########Shiny application##########
#####################################

ui <- fluidPage(

  titlePanel("Industry space"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select region, year and time lag."),
      
      selectInput("plot.region", label = "Region", 
                  choices = region.list),
      
      radioButtons("plot.type", label = "Plot type",
                   choices = list("Start" = 1, "Change" = 2, "Density" = 3),
                   selected = 1),
      
      uiOutput("ui")
    ),
  
  mainPanel(
    plotOutput("p.network", height = "50vw",
               hover = hoverOpts("plot_hover", delay = 50, delayType = "debounce")),
    uiOutput("tooltip")
    )
  )
)


server <- function(input, output) {
  
  plot.type <- reactive({input$plot.type})
  
  output$ui <- renderUI({
    if (input$plot.type == 1) {
      sliderInput("plot.year", label = "Year", min = 2006, max = 2013, value = 2006, sep = "", animate = T)
    } else {
      sliderInput("plot.year", label = "Year", min = 2006, max = 2013, value = c(2006, 2010), sep = "", animate = T)
    }
  })
  
  plot.region <- reactive({tolower(substr(input$plot.region, 1, 4))})
  
  #plot.year1 <- reactive({ifelse(length(input$plot.year) == 1, input$plot.year, input$plot.year[1])})
  #plot.year2 <- reactive({ifelse(length(input$plot.year) == 1, 2013, input$plot.year[2])})
  
  plot.year1 <- reactive({
    if (is.null(input$plot.year)) {
      2006
    } else if (length(input$plot.year) == 1) {
      input$plot.year
    } else {
      input$plot.year[1]
    }})
  
  plot.year2 <- reactive({
    if (length(input$plot.year) == 1) {
      2013
    } else {
      input$plot.year[2]
    }})
  
  output$p.network <- renderPlot({
    plot.network(plot.type(), plot.region(), plot.year1(), plot.year2())
  })
  
  hover.df <- reactive({get(paste0("ggnet.", plot.region()))})
  
  output$tooltip <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(hover.df(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_px <- hover$coords_css$x + 5
    top_px <- hover$coords_css$y + 5
    
    wellPanel(paste0(point$vertex.name, ": ", industry.label(point$vertex.name)),
              style = paste0("position:absolute; z-index:100;left:", left_px, "px; top:", top_px, "px;"))
  })
  
}

shinyApp(ui = ui, server = server)



