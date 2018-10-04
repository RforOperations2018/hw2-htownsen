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
# Grab the unique values for the FamiliarityTechnoology column (yes, it is spelled wrong in the data), which is used in INPUT 3
familiarTech <- sort(ckanUniques("6d29ac78-12b8-4e1d-b325-6edeef59b593", "FamiliarityTechnoology")$FamiliarityTechnoology)
# Grab the unique values for the SafetyAV column, which is used in INPUT 1
safetySlide <- sort(ckanUniques("6d29ac78-12b8-4e1d-b325-6edeef59b593", "SafetyAV")$SafetyAV)


# Define UI for application 
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
                            # Making sure NA is not included as a choice
                            choices=familiarTech[familiarTech!= ""],
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

# Define server logic 
server <- function(input, output, session=session) {
  
  loaddf <- eventReactive(input$button, {
    # Build API Query with proper encodes
    
    # Also filter by the three inputs 
    # Using gsub to deal with spaces for certain factor levels like "Not familiar at all"
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%226d29ac78-12b8-4e1d-b325-6edeef59b593%22%20WHERE%20%22SafetyAV%22%3E%3D%27",
                  input$safetySelect[1], "%27%20AND%20%22SafetyAV%22%3C%3D%27",input$safetySelect[2],"%27%20AND%20%22FeelingsProvingGround%22%20IN%20%28%27", gsub(" ", "%20",input$feelSelect[1]),
                  "%27%2C%20%27", gsub(" ", "%20",input$feelSelect[2]),"%27%2C%20%27", gsub(" ", "%20",input$feelSelect[3]),"%27%2C%20%27", gsub(" ", "%20",input$feelSelect[4]), "%27%2C%20%27",
                  gsub(" ", "%20",input$feelSelect[5]), "%27%29%20AND%20%22FamiliarityTechnoology%22%20IN%20%28%27", gsub(" ", "%20", input$techSelect[1]),"%27%2C%20%27", gsub(" ", "%20", input$techSelect[2]),
                  "%27%2C%20%27", gsub(" ", "%20", input$techSelect[3]), "%27%2C%20%27", gsub(" ", "%20", input$techSelect[4]),"%27%2C%20%27", gsub(" ", "%20", input$techSelect[5]),"%27%29"
    )
    
    datav <- ckanSQL(url) %>% 
      mutate(SafetyRating = as.numeric(SafetyAV),
             # Use a period if there's a space in the column name
             SurveyDate = as.Date(End.Date),
             ProvingGroundFeel = FeelingsProvingGround,
             AVTechFamiliarity = FamiliarityTechnoology)
    
    return(datav)
  })
  
  #PLOT 1: Vertical Bar plot showing the number of respondents who feel a certain way about proving ground
  output$plot1 <- renderPlotly({

    dat <- loaddf()
    # data for plot 1
    df <- dat %>%
      group_by(FeelingsProvingGround) %>%
      summarise(COUNT = n())

    ggplotly(
      ggplot(data = dat, aes(x = FeelingsProvingGround, color = FeelingsProvingGround, fill=FeelingsProvingGround)) +
        geom_bar() + ylab("Number of Respondents") + ggtitle("How do you feel about the use of Pittsburgh's public streets as a proving ground for AVs?") +
        guides(color = FALSE))
  })
  
  # PLOT 2: Horizontal Bar plot showing respondent familiarity with AV technologies
  output$plot2 <- renderPlotly({
    dat <- loaddf()
    ggplotly(
      ggplot(data = dat, aes(x = FamiliarityTechnoology, color = FamiliarityTechnoology, fill = FamiliarityTechnoology)) +
        geom_bar() + ggtitle("How familiar are you with the technology behind autonomous vehicles?") +
        guides(color = FALSE) + xlab("Familiarity") + ylab("Number of Respondents") + coord_flip())
  })

  # FIGURE/PLOT 3: Word Cloud of all the Zipcodes represented, given the inputs
  output$plot3 <- renderWordcloud2({
    v <- loaddf()
    t <- table(v$ZipCode)
    w <- as.data.frame(t)
    names(w)[1] <- "word"
    names(w)[2] <- "freq"
    wordcloud2(w)
  })

  # Data Table Output containing information from the input fields
  output$table <- DT::renderDataTable({
    df <- loaddf()
    subset(df, select = c(SafetyRating, SurveyDate, ProvingGroundFeel, AVTechFamiliarity, ZipCode))
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
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "feelSelect", selected = c("Approve", "Somewhat Approve", "Neutral"))
    updateSliderInput(session, "safetySelect", value = c(min(as.numeric(safetySlide), na.rm=T), max(as.numeric(safetySlide), na.rm=T)))
    updateCheckboxGroupInput(session, "techSelect", selected = c("Mostly familiar"))
    showNotification("You have successfully reset the filters! Make sure to hit Submit again!", type = "message")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
