#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##########################################################################################################################
# HW2 Instructions:
# Directions: Use plotly (with or without the aid of ggplot2) to create three 
# (3) different kinds of figures and one (1) data table. Include at least three (3) types of inputs, 
#one (1) functioning downloadButton() and one (1) observer in the server. On the server side your plots and 
#tables must utilize the reactive function for any and all datasets.

# loading necessary libraries
library(shiny)
library(plyr)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)

# Loading in the data for this app.
# This data was downloaded from the WPRDC.
# BikePGH -> Autonomous Vehicle Survey of Bicyclists and Pedestrians in Pittsburgh, 2017

# No need to set working directory since this app2.R file resides in the same location/repository
# as the csv files
df = read.csv("bikepghmembers.csv", strip.white = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("BikePGH Member Responses to AV Survey"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(type="tabs",
                            tabPanel("Plots", plotlyOutput()),
                            tabPanel("Data Table", plotlyOutput())
      )
   )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output, session=session) {
   
   #output$distPlot <- renderPlot({

   #})
}

# Run the application 
shinyApp(ui = ui, server = server)

