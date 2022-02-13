# sample R + Shiny example for CS 424 Spring 2022 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)

# assume all of the tsv files in this directory are data of the same kind that I want to visualize

temp = list.files(pattern="*.tsv")
tableTrainData <- lapply(temp, read.delim)
rawTrainData <- do.call(rbind, tableTrainData)
na.omit(format(dmy(rawTrainData$date), "%m-%d-%Y")) #removes all NA dates
rawTrainData$date <- as.Date(rawTrainData$date, "%m/%d/%Y") #re-formats everything and then changes <char> to <date>

years<-c(2001:2021)
stopNames <- c("UIC-Halsted", "O\'Hare Airport", "Cermak-Chinatown")
xAxisData <- c("Day of the Week", "Day", "Month", "All Years")

ui <- navbarPage("Project 1: CTA Ridership",
                 position = c("fixed-bottom"),
   tabPanel("Graphs",
            
            # Sidebar panel for inputs ----
            sidebarPanel(
              selectInput("Year", "Graph 1: Select the year to visualize", years, selected = 2021),
              selectInput("StopName", "Graph 1: Select a stop to visualize", stopNames, selected = "UIC-Halsted"),
              selectInput("X_Axis_Data", "Graph 1: Select a different chart to visualize", xAxisData, selected = "All Years"),
              
              selectInput("Year_2", "Graph 2: Select the year to visualize", years, selected = 2021),
              selectInput("StopName_2", "Graph 2: Select a stop to visualize", stopNames, selected = "O\'Hare Airport"),
              selectInput("X_Axis_Data_2", "Graph 2: Select a different chart to visualize", xAxisData, selected = "All Years"),
              
              selectInput("Year_3", "Graph 3: Select the year to visualize", years, selected = 2021),
              selectInput("StopName_3", "Graph 3: Select a stop to visualize", stopNames, selected = "Cermak-Chinatown"),
              selectInput("X_Axis_Data_3", "Graph 3: Select a different chart to visualize", xAxisData, selected = "All Years")
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
              column(6,
                     fluidRow(
                       box
                       (
                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                         plotOutput("hist1", height = 500)
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Graph 3: ", solidHeader = TRUE, status = "primary", width = 12,
                         plotOutput("hist3", height = 500)
                       )
                     )
              ),
              column(6, 
                     fluidRow(
                       box(
                         title = "Graph 2: ", solidHeader = TRUE, status = "primary", width = 12,
                         plotOutput("hist2", height = 500)
                       )
                     )
              )
            )
      ),
   tabPanel(
     "About",
     column(5, 
            tags$h1("About"), 
            tags$h2("Code Written By: Raphael Genova During The Spring 2022 Semester @ UIC"),
            tags$h2("Where CTA Ridership data came from: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
            tags$h2("Why Was This Created: This app was created to better visualize the ridership over the years at the UIC-Halsted, O\'Hare Airport and Cermak-Chinatown stops.")
        )
     )
)

#All things output related ----
server <- function(input, output) {
  justOneStopReactive_ALL_YEARS <- reactive({subset(rawTrainData, rawTrainData$stationname == input$StopName)})
  justOneStopReactive_ALL_YEARS_2 <- reactive({subset(rawTrainData, rawTrainData$stationname == input$StopName_2)})
  justOneStopReactive_ALL_YEARS_3 <- reactive({subset(rawTrainData, rawTrainData$stationname == input$StopName_3)})
  
  justOneStopReactive <- reactive({subset(rawTrainData, rawTrainData$stationname == input$StopName & year(rawTrainData$date) == input$Year)})
  justOneStopReactive_2 <- reactive({subset(rawTrainData, rawTrainData$stationname == input$StopName_2 & year(rawTrainData$date) == input$Year_2)})
  justOneStopReactive_3 <- reactive({subset(rawTrainData, rawTrainData$stationname == input$StopName_3 & year(rawTrainData$date) == input$Year_3)})
  
  output$hist1 <- renderPlot({
    justOneStop <- justOneStopReactive()
    justOneStop_ALL_YEARS <- justOneStopReactive_ALL_YEARS()
    
    titleBuild <- paste(input$StopName, "Rides per", input$X_Axis_Data, "in", input$Year)
    
    if(input$X_Axis_Data == "Day of the Week")
    {
      ggplot(data=justOneStop, aes(wday(ymd(justOneStop$date)), justOneStop$rides, fill=factor(wday(ymd(justOneStop$date))))) +
        geom_bar(stat="identity") + labs(x = "Day of the Week", y = "Rides", title = titleBuild, fill="Day of the Week")
    }
    else if(input$X_Axis_Data == "Day")
    {
      ggplot(data=justOneStop, aes(day(ymd(justOneStop$date)), justOneStop$rides, fill=factor(day(ymd(justOneStop$date))))) +
        geom_bar(stat="identity") + labs(x = "Days", y = "Rides", title = titleBuild, fill="Days")
    }
    else if(input$X_Axis_Data == "Month")
    {
      ggplot(data=justOneStop, aes(month(ymd(justOneStop$date)), justOneStop$rides, fill=factor(month(ymd(justOneStop$date))))) +
        geom_bar(stat="identity") + labs(x = "Months", y = "Rides", title = titleBuild, fill="Months")
    }
    else if(input$X_Axis_Data == "All Years")
    {
      titleBuild_All <- paste(input$StopName, "Rides for", input$X_Axis_Data)
      ggplot(data=justOneStop_ALL_YEARS, aes(year(ymd(justOneStop_ALL_YEARS$date)), justOneStop_ALL_YEARS$rides, fill=factor(year(ymd(justOneStop_ALL_YEARS$date))))) +
        geom_bar(stat="identity") + labs(x = "Years", y = "Rides", title = titleBuild_All, fill="Year")
    }
  })
  
  output$hist2 <- renderPlot({
    justOneStop_2 <- justOneStopReactive_2()
    justOneStop_ALL_YEARS_2 <- justOneStopReactive_ALL_YEARS_2()
    
    titleBuild <- paste(input$StopName_2, "Rides per", input$X_Axis_Data_2, "in", input$Year_2)
    
    if(input$X_Axis_Data_2 == "Day of the Week")
    {
      ggplot(data=justOneStop_2, aes(wday(ymd(justOneStop_2$date)), justOneStop_2$rides, fill=factor(wday(ymd(justOneStop_2$date))))) +
        geom_bar(stat="identity") + labs(x = "Day of the Week", y = "Rides", title = titleBuild, fill="Day of the Week")
    }
    else if(input$X_Axis_Data_2 == "Day")
    {
      ggplot(data=justOneStop_2, aes(day(ymd(justOneStop_2$date)), justOneStop_2$rides, fill=factor(day(ymd(justOneStop_2$date))))) +
        geom_bar(stat="identity") + labs(x = "Days", y = "Rides", title = titleBuild, fill="Days")
    }
    else if(input$X_Axis_Data_2 == "Month")
    {
      ggplot(data=justOneStop_2, aes(month(ymd(justOneStop_2$date)), justOneStop_2$rides, fill=factor(month(ymd(justOneStop_2$date))))) +
        geom_bar(stat="identity") + labs(x = "Months", y = "Rides", title = titleBuild, fill="Months")
    }
    else if(input$X_Axis_Data_2 == "All Years")
    {
      titleBuild_All <- paste(input$StopName_2, "Rides for", input$X_Axis_Data_2)
      ggplot(data=justOneStop_ALL_YEARS_2, aes(year(ymd(justOneStop_ALL_YEARS_2$date)), justOneStop_ALL_YEARS_2$rides, fill=factor(year(ymd(justOneStop_ALL_YEARS_2$date))))) +
        geom_bar(stat="identity") + labs(x = "Years", y = "Rides", title = titleBuild_All, fill="Year")
    }
  })
  
  output$hist3 <- renderPlot({
    justOneStop_3 <- justOneStopReactive_3()
    justOneStop_ALL_YEARS_3 <- justOneStopReactive_ALL_YEARS_3()
    
    titleBuild <- paste(input$StopName_3, "Rides per", input$X_Axis_Data_3, "in", input$Year_3)
    
    if(input$X_Axis_Data_3 == "Day of the Week")
    {
      ggplot(data=justOneStop_3, aes(wday(ymd(justOneStop_3$date)), justOneStop_3$rides, fill=factor(wday(ymd(justOneStop_3$date))))) +
        geom_bar(stat="identity") + labs(x = "Day of the Week", y = "Rides", title = titleBuild, fill="Day of the Week")
    }
    else if(input$X_Axis_Data_3 == "Day")
    {
      ggplot(data=justOneStop_3, aes(day(ymd(justOneStop_3$date)), justOneStop_3$rides, fill=factor(day(ymd(justOneStop_3$date))))) +
        geom_bar(stat="identity") + labs(x = "Days", y = "Rides", title = titleBuild, fill="Days")
    }
    else if(input$X_Axis_Data_3 == "Month")
    {
      ggplot(data=justOneStop_3, aes(month(ymd(justOneStop_3$date)), justOneStop_3$rides, fill=factor(month(ymd(justOneStop_3$date))))) +
        geom_bar(stat="identity") + labs(x = "Months", y = "Rides", title = titleBuild, fill="Months")
    }
    else if(input$X_Axis_Data_3 == "All Years")
    {
      titleBuild_All <- paste(input$StopName_3, "Rides for", input$X_Axis_Data_3)
      ggplot(data=justOneStop_ALL_YEARS_3, aes(year(ymd(justOneStop_ALL_YEARS_3$date)), justOneStop_ALL_YEARS_3$rides, fill=factor(year(ymd(justOneStop_ALL_YEARS_3$date))))) +
        geom_bar(stat="identity") + labs(x = "Years", y = "Rides", title = titleBuild_All, fill="Year")
    }
  })
}
# shiny launcher ----
shinyApp(ui = ui, server = server)