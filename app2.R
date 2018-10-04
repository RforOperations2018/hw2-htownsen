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

#Homework 4: Using Web API’s
#Due Date: 10/5/2018

#For many web applications it is important to pull information from somewhere else on the internet.
#For this assignment Students are expected to change their data source from a static file in their 
#code from Homework 2 or Project 1 and replace it with the API from that resource.
#Students should also address any issues brought up during the grading the assignment in question. 


# loading necessary libraries
library(shiny)
library(plyr)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(wordcloud2)
# New libraries for HW 4
library(httr)
library(jsonlite)
library(htmltools)
# library(gsubfn)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  # trying to get rid of the empty strings too
  json <- gsub("NaN|''", 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

# BikePGH Member Responses from Autonomous Vehicle Survey
# Grab the unique values for the FeelingsProvingGround column, which is used in INPUT 2
feelPG <- sort(ckanUniques("6d29ac78-12b8-4e1d-b325-6edeef59b593", "FeelingsProvingGround")$FeelingsProvingGround)
# Grab the unique values for the FamiliarityTechnoology column, which is used in INPUT 3
familiarTech <- sort(ckanUniques("6d29ac78-12b8-4e1d-b325-6edeef59b593", "FamiliarityTechnoology")$FamiliarityTechnoology)
# Grab the unique values for the FamiliarityTechnoology column, which is used in INPUT 3
safetySlide <- sort(ckanUniques("6d29ac78-12b8-4e1d-b325-6edeef59b593", "SafetyAV")$SafetyAV)

# BikePGH -> Autonomous Vehicle Survey of Bicyclists and Pedestrians in Pittsburgh, 2017

# No need to set working directory since this app2.R file resides in the same location/repository
# as the csv files
# df.load = read.csv("bikepghmembers.csv", strip.white = T)
# 
# # Rename a few columns to make more sense
# names(df.load)[20] <- "TechnologyFamiliarity"
# 
# # Making zipcodes a string for the wordcloud
# df.load$ZipCode <- as.character(df.load$ZipCode)
# 
# # Casting the "SafetyAV" scale as a numeric
# df.load$SafetyAV <- as.numeric(df.load$SafetyAV)
# 
# pdf(NULL)

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
                    min = min(as.numeric(safetySlide), na.rm=T),
                    max = max(as.numeric(safetySlide), na.rm=T),
                    value = c(min(as.numeric(safetySlide), na.rm=T), max(as.numeric(safetySlide), na.rm=T)),
                    step = 1),
        
        # INPUT 2: Feelings toward AV Proving Ground in PGH Selection
        selectInput("feelSelect",
                    "Select Respondent Feelings toward AV Proving Ground in PGH:",
                    choices = feelPG,
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
  
  loaddf <- eventReactive(input$button, {
    # Build API Query with proper encodes
    #I only want to keep COMPLETE surveys, so filter out the INCOMPLETE as well
    
    #SELECT+*+FROM+%226d29ac78-12b8-4e1d-b325-6edeef59b593%22+WHERE+%22SafetyAV%22+%3E%3D+'1'+AND+%22SafetyAV%22+%3C%3D+'5'+AND+%22FeelingsProvingGround%22+IN+
    #(list_vals)+AND+%22FamiliarityTechnoology%22+IN+(list_vals)%3Fsql%3D
    #"%20%AND%20%22FeelingsProvingGround%22%20IN%20", input$feelSelect,
    #"%20AND%20%22FamiliarityTechnoology%22%20IN%20", input$techSelect, "%3Fsql%3D")
    #paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%226d29ac78-12b8-4e1d-b325-6edeef59b593%22%20WHERE%20%22SafetyAV%22%203E%3D",
    #input$safetySelect[1], "%20AND%20%22SafetyAV%22%20%3C%3D", input$safetySelect[2],"%3Fsql%3D")
    
    # Also filter by the three inputs 
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%226d29ac78-12b8-4e1d-b325-6edeef59b593%22%20WHERE%20%22SafetyAV%22%3E%3D%27",
                  1, "%27%20AND%20%22SafetyAV%22%3C%3D%27",5,"%27"
    )
    
    datav <- ckanSQL(url) %>% 
      mutate(rating = as.numeric(SafetyAV))
    
    return(datav)
  })
  
  # # Filtering the survey data
  # dfInput <- eventReactive(input$button, {
  #   df <- df.load %>%
  #     # safetySelect filter for range on scale
  #     filter(SafetyAV >= input$safetySelect[1] & SafetyAV <= input$safetySelect[2])
  #     # feelSelect (feelings toward having PGH as an AV Proving Ground) Filter
  #     if (length(input$feelSelect) > 0 ) {
  #       df <- subset(df, FeelingsProvingGround %in% input$feelSelect)
  #     }
  #   # techSelect filter for checkboxes
  #   if (length(input$techSelect) > 0 ) {
  #     df <- subset(df, TechnologyFamiliarity %in% input$techSelect)
  #   }
  #   return(df)
  # })
  
  # PLOT 1: Vertical Bar plot showing the number of respondents who feel a certain way about proving ground
  # output$plot1 <- renderPlotly({
  #   
  #   dat <- loaddf()
  #   # data for plot 1
  #   df <- dat %>%
  #     group_by(FeelingsProvingGround) %>%
  #     summarise(COUNT = n())
  #   
  #   ggplotly(
  #     ggplot(data = dat, aes(x = FeelingsProvingGround, color = FeelingsProvingGround, fill=FeelingsProvingGround)) +
  #       geom_bar() + ggtitle("How do you feel right now about the use of Pittsburgh's public streets as a proving ground for AVs?") +
  #       guides(color = FALSE))
  # })
  
  # # PLOT 2: Horizontal Bar plot showing respondent familiarity with AV technologies
  # output$plot2 <- renderPlotly({
  #   dat <- dfInput()
  #   ggplotly(
  #     ggplot(data = dat, aes(x = TechnologyFamiliarity, color = TechnologyFamiliarity, fill=TechnologyFamiliarity)) +
  #       geom_bar() + ggtitle("How familiar are you with the technology behind autonomous vehicles?") +
  #       guides(color = FALSE) + coord_flip())
  # })

  # # FIGURE/PLOT 3: Word Cloud of all the Zipcodes represented, given the inputs
  # output$plot3 <- renderWordcloud2({
  #   v <- dfInput()
  #   t <- table(v$ZipCode)
  #   w <- as.data.frame(t)
  #   names(w)[1] <- "word"
  #   names(w)[2] <- "freq"
  #   wordcloud2(w)
  # })

  # Data Table Output
  #FeelingsProvingGround, AVSafetyPotential, PayingAttentionAV, TechnologyFamiliarity
  output$table <- DT::renderDataTable({
    df <- loaddf()
    subset(df, select = c(rating))
  })

  # Download data in the datatable
  # Must "Open in Browser" (the app) in order for the download to work as expected
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BikePGH-survey-AV-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(loaddf(), file)
    }
  )
  # # Reset Filter Data
  # observeEvent(input$reset, {
  #   updateSelectInput(session, "feelSelect", selected = c("Approve", "Somewhat Approve", "Neutral"))
  #   updateSliderInput(session, "safetySelect", value = c(min(df.load$SafetyAV, na.rm = T), max(df.load$SafetyAV, na.rm = T)))
  #   updateCheckboxGroupInput(session, "techSelect", selected = c("Extremely familiar", "Mostly familiar"))
  #   showNotification("You have successfully reset the filters!", type = "message")
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
