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

# Rename a few columns to make more sense
names(df)[20] <- "TechnologyFamiliarity"

# Create a new column that is End.Date minus Start.Date and call it "CompleteTime"
# And then use "CompleteTime" in an input slider
# Unfortunately, these times are recorded in "AM" and "PM" instead of military time. All have PST
df$time2 <- strptime(df$time2, "%Y-%m-%d %H:%M:%OS")

df$CompleteTime <- as.Date(as.character(df$End.Date), format="%m/%d/%Y %h:%m:%s %p")-
  as.Date(as.character(df$Start.Date), format="%m/%d/%Y %I:%m:%s %p %X")

#df$CompleteTime <- difftime(df$End.Date,df$Start.Date,units="mins")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("BikePGH Member Responses to AV Survey"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("feelselect",
                    "Select Feelings toward Proving Ground:",
                    choices = sort(unique(df$FeelingsProvingGround)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("Approve", "Somewhat Approve"))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(type="tabs",
                            tabPanel("Plots", plotlyOutput("plot1")),
                            tabPanel("Table",
                                     inputPanel(
                                       downloadButton("downloadData","Download Survey Data")
                                     ),
                                     fluidPage(DT::dataTableOutput("table")))
      )
   )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output, session=session) {
  
  # Filtered survey data
  dfInput <- reactive({
    df <- starwars.load %>%
      # Slider Filter
      filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    # feelSelect (feelings toward having PGH as an AV Proving Ground) Filter
    if (length(input$worldSelect) > 0 ) {
      starwars <- subset(starwars, homeworld %in% input$worldSelect)
    }
    
    return(starwars)
  })
  # Data Table
  output$table <- DT::renderDataTable({
    starwars <- dfInput()
    subset(df, select = c(End.Date, FeelingsProvingGround, AVSafetyPotential, PayingAttentionAV, TechnologyFamiliarity))
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BikePGH-survey-AV-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

