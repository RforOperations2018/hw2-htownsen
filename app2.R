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
library(wordcloud2)

# Loading in the data for this app.
# This data was downloaded from the WPRDC.
# BikePGH -> Autonomous Vehicle Survey of Bicyclists and Pedestrians in Pittsburgh, 2017

# No need to set working directory since this app2.R file resides in the same location/repository
# as the csv files
# Use <- in the future, see code standards
df.load <- read.csv("bikepghmembers.csv", strip.white = T)

# Rename a few columns to make more sense
names(df.load)[20] <- "TechnologyFamiliarity"

# Making zipcodes a string for the wordcloud
df.load$ZipCode <- as.character(df.load$ZipCode)

# Casting the "SafetyAV" scale as a numeric
df.load$SafetyAV <- as.numeric(df.load$SafetyAV)

pdf(NULL)

# Create a new column that is End.Date minus Start.Date and call it "CompleteTime"
# And then use "CompleteTime" in an input slider
# Unfortunately, these times are recorded in "AM" and "PM" instead of military time. All have PST
# df$time2 <- strptime(df$time2, "%Y-%m-%d %H:%M:%OS")
# 
# df$CompleteTime <- as.Date(as.character(df$End.Date), format="%m/%d/%Y %h:%m:%s %p")-
#   as.Date(as.character(df$Start.Date), format="%m/%d/%Y %I:%m:%s %p %X")

#df$CompleteTime <- difftime(df$End.Date,df$Start.Date,units="mins")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
   
   # Application title
   titlePanel("BikePGH Member Responses to AV Survey"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # INPUT 1: AV Safety Selection
        sliderInput("safetySelect",
                    "Select Respondent Safety Perceptions for AVs (from 1 'very unsafe' to 5 'very safe'):",
                    min = min(df.load$SafetyAV, na.rm = T),
                    max = 5,
                    value = c(min(df.load$SafetyAV, na.rm = T), 5),
                    step = 1),
        
        # INPUT 2: Feelings toward AV Proving Ground in PGH Selection
        selectInput("feelSelect",
                    "Select Respondent Feelings toward AV Proving Ground in PGH:",
                    choices = sort(unique(df.load$FeelingsProvingGround)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("Approve", "Somewhat Approve", "Neutral")),
        
        # INPUT 3: Familiarity with AV Technologies
         checkboxGroupInput("techSelect", label="Select Respondent Familiarity with AV Techs:",
                            choices=c(
                              "Extremely Familiar" = "Extremely familiar",
                              "Mostly Familiar" = "Mostly familiar",
                              "Somewhat Familiar" = "Somewhat familiar",
                              "Mostly Unfamiliar" = "Mostly Unfamiliar",
                              "Not Familiar at All" = "Not familiar at all"),
                            selected = c("Extremely familiar", "Mostly familiar")),
        actionButton("reset", "Reset Filters", icon = icon("refresh")),
        actionButton("button", "SUBMIT")
                            
      ),
      
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(type="tabs",
                            tabPanel("Plots", fluidRow(plotlyOutput("plot1"), fluidRow(plotlyOutput("plot2")), fluidRow(wordcloud2Output("plot3")))),
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
  
  # Filtering the survey data
  dfInput <- eventReactive(input$button, {
    df <- df.load %>%
      # safetySelect filter for range on scale
      filter(SafetyAV >= input$safetySelect[1] & SafetyAV <= input$safetySelect[2])
      # feelSelect (feelings toward having PGH as an AV Proving Ground) Filter
      if (length(input$feelSelect) > 0 ) {
        df <- subset(df, FeelingsProvingGround %in% input$feelSelect)
      }
    # techSelect filter for checkboxes
    if (length(input$techSelect) > 0 ) {
      df <- subset(df, TechnologyFamiliarity %in% input$techSelect)
    }
    return(df)
  })
  
  # PLOT 1: Vertical Bar plot showing the number of respondents who feel a certain way about proving ground
  output$plot1 <- renderPlotly({
    dat <- dfInput()
    ggplotly(
      # In the future try and make a custom tooltip
      ggplot(data = dat, aes(x = FeelingsProvingGround, color = FeelingsProvingGround, fill=FeelingsProvingGround)) +
        geom_bar() + ggtitle("How do you feel right now about the use of Pittsburgh's public streets as a proving ground for AVs?") +
        guides(color = FALSE))
  })
  
  # PLOT 2: Horizontal Bar plot showing respondent familiarity with AV technologies
  output$plot2 <- renderPlotly({
    dat <- dfInput()
    ggplotly(
      ggplot(data = dat, aes(x = TechnologyFamiliarity, color = TechnologyFamiliarity, fill=TechnologyFamiliarity)) +
        geom_bar() + ggtitle("How familiar are you with the technology behind autonomous vehicles?") +
        guides(color = FALSE) + coord_flip())
  })
  
  # FIGURE/PLOT 3: Word Cloud of all the Zipcodes represented, given the inputs
  output$plot3 <- renderWordcloud2({
    v <- dfInput()
    t <- table(v$ZipCode)
    w <- as.data.frame(t)
    names(w)[1] <- "word"
    names(w)[2] <- "freq"
    wordcloud2(w)
  })
  
  # Data Table Output
  output$table <- DT::renderDataTable({
    df <- dfInput()
    subset(df, select = c(End.Date, FeelingsProvingGround, AVSafetyPotential, PayingAttentionAV, TechnologyFamiliarity))
  })
  
  # Download data in the datatable
  # Must "Open in Browser" (the app) in order for the download to work as expected
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BikePGH-survey-AV-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dfInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "feelSelect", selected = c("Approve", "Somewhat Approve", "Neutral"))
    updateSliderInput(session, "safetySelect", value = c(min(df.load$SafetyAV, na.rm = T), max(df.load$SafetyAV, na.rm = T)))
    updateCheckboxGroupInput(session, "techSelect", selected = c("Extremely familiar", "Mostly familiar"))
    showNotification("You have successfully reset the filters!", type = "message")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
