#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(dbplyr)
library(datasets)


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Upload data file", icon = icon("th"), tabName = "data", badgeColor = "green"),
        menuItem("Data Table", icon = icon("th"), tabName = "dataTable", badgeColor = "green"),
        menuItem("Leaflet Map", icon = icon("th"), tabName = "leaflet", badgeColor = "green")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "data",
                ui <- fluidPage(
                    
                    # App title ----
                    titlePanel("Uploading Files"),
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                            
                            # Input: Select a file ----
                            fileInput(inputId = "file1", "Choose file:",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Checkbox if file has header ----
                            checkboxInput("header", "Header", TRUE),
                            
                            # Input: Select separator ----
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            
                            # Input: Select quotes ----
                            radioButtons("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"'),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select number of rows to display ----
                            radioButtons("disp", "Display",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head")
                            
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                            
                            # Output: Data file ----
                            tableOutput("contents")
                            
                        )
                        
                    )
                )
        ),
        tabItem(
            tabName = "leaflet",
            fluidPage(
                leafletOutput(height = 800,"mymap"),
                p()
            )
        ),
        tabItem(
            tabName = "dataTable",
            fluidPage(
              titlePanel("Basic DataTable"),
              
              #attempt to have conditional checkboxes for data:
              
              #sidebarPanel(
              #conditionalPanel(
              # 'input.dataset === "df"',
              # checkboxGroupInput("show_vars", "Columns to show:",
              #                   names(df), selected = names(df)))
              
              #),
              mainPanel(
              tabsetPanel(
              id='dataset',
              tabPanel("df", DT::dataTableOutput("mytable1"))
             
              )
            ))
            ),
        tabItem(
          tabName = "anothertab",
          fluidPage(
            titlePanel("Telephones by region"),
            # Generate a row with a sidebar
            sidebarLayout(      
              # Define the sidebar with one input
              sidebarPanel(
                selectInput("region", "Region:", 
                            choices=colnames(WorldPhones)),
                hr(),
                helpText("Data from AT&T (1961) The World's Telephones.")
              ),
              
              # Create a spot for the barplot
              mainPanel(
                plotOutput("phonePlot")  
              )
            
          )
        )
    )))


# Put them together into a dashboardPage

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(dashboardHeader(title="App"),
                    sidebar = sidebar,
                    body = body)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       )
        
        #separate(df,col=`GPS location (click icon to view map & confirm)`,into=c("lat","long"), sep="|")
        
        
        
        if(input$disp == "head") {
          
            return(head(df))
        }
        else {
          
            return(df)
        }
        
        
    })
    #Leaflet map server side
    #Leaflet map render output
   
      output$mymap <- renderLeaflet({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       )
        
        
        #function chooses icons based on vessel type
        getIcon <- function(df) {
          sapply(df$vessel, function(vessel) {
            if(vessel == "Powerboat") {
              "ship"
            } else if(vessel == "ski") {
              "fighter-jet"
            }else if(vessel == "Paddler"){
              "compass"
            } else {
              "anchor"
            } })
        }
        
        #applies coloring based on if warning has been issued
        getColor <- function(df) {
          sapply(df$warning, function(warning) {
            if(warning == "yes") {
              "red"
            } else if(warning == "no") {
              "green"
            } else {
              "black"
            } })
        }
        
        icons <- awesomeIcons(
          icon = getIcon(df),
          iconColor = 'black',
          library = 'fa',
          markerColor = getColor(df)
        )
        
        #trying to separate columns into latitude and longitude for use in leaflet map
        #df <- sep = separate(df,col=df$`GPS location (click icon to view map & confirm)`,into=c("lat","long"), sep="|")
        
        leaflet(df,options = leafletOptions(minZoom = 4)) %>%
             addProviderTiles("OpenStreetMap")%>%
             setView(lng = 172.6398470,lat = -43.525650, zoom = 6)%>%
             addAwesomeMarkers(~longitude, ~latitude,icon=icons ,popup = paste("Vehicle: ", df$vessel, "<br>",
                                                             "No. People:", df$people, "<br>",
                                                             "Infringement:", df$warning, "<br>",
                                                             "Lat:", df$latitude, "<br>",
                                                             "Long:", df$longitude, "<br>",
                                                             "Form completed by:", df$Submitted, "<br>",
                                                             "Time:", df$Time))
            
        
      
    })
      
      # Filter data table based on selections

      
      
      output$mytable1 <- DT::renderDataTable({
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        #Was trying to implement checkboxes for displaying columns
        #DT::datatable(df[, input$show_vars, drop = FALSE])
        
        df
      })
      
    
}


# Run the application 
shinyApp(ui = ui, server = server)
