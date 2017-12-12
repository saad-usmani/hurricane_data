library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)

###Data Import and Simple Cleaning
hur <- fread(file.path("C:/Users/susmani/Documents/Year 5/Courses/Data Munging/GroupProject2/hurricane.csv"),na.strings = c("PrivacySuppressed", "NULL"))
hur<-data.frame(hur)
hur$Date<-ymd(hur$Date)
hur$Latitude<-as.numeric(unlist(strsplit(hur$Latitude, split='N', fixed=TRUE)))
hur$Longitude<-as.numeric(unlist(strsplit(hur$Longitude, split='W', fixed=TRUE)))
hur$Longitude<-(-1)*hur$Longitude
hur_start<-hur[!duplicated(hur$ID), ] #Dataset of only origin of systems.
atlantic_map<-get_map(location = "puerto rico", zoom = 4)
###Actual App Below

years<-c(1851:2015)



Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoic3VzbWFuaSIsImEiOiJjamFhZnJlb3YwczdmMzJxaXlmcHJ0ZGZ2In0.vdCC--CzL7cM-XsG8yCrFw')



ui<-fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("Hurricane Exploration", 
             tabPanel("Origin",
                      sidebarPanel(
                        tags$div(
                          tags$p("Choose a system by its year and name and discover all other similar hurricanes with similar origins.")),
                        tags$head(tags$style("#origin{height:100vh !important;}")),
                        selectInput(inputId = "year", label = "Year", as.list(years)),
                        uiOutput('names'),
                        #textInput(inputId = "name", label = "Name of Storm", "Name")
                        actionButton("goButton", "Go!"), 
                        width = 2
                      ),
                      mainPanel(
                        plotlyOutput("origin"), width = 10
                      )
                      
             ),
             tabPanel("Heat Maps",
                      sidebarPanel(
                        tags$div(
                          tags$p("Creates a heatmap.")),
                        tags$head(tags$style("#origin{height:100vh !important;}")), width = 2
                      ), 
                      tabsetPanel(
                        tabPanel(plotOutput("heat"), width = 10),
                        tabPanel(plotOutput("hj"))
                      )
             ),
             tabPanel("Closest Paths",
                      sidebarLayout(
                        sidebarPanel(
                          tags$div(
                            tags$p("Finding top 5 closest tracks.")),
                          tags$head(tags$style("#tracks{height:100vh !important;}")),
                          tableOutput("table"), width = 2),
                        mainPanel(plotlyOutput("tracks"), width = 10))
             ),
             tabPanel("Path Finder",
                      sidebarPanel(
                        tags$div(
                          tags$p("Creates a heatmap.")),
                        tags$head(tags$style("#origin{height:100vh !important;}")), width = 2
                      ),
                      mainPanel(
                        plotlyOutput("predict"), width = 10
                      )
             )
             
  )
)


server<- function(input, output){
  output$names<-renderUI({
    butt<-reactive({unique(hur$Name[hur$Date>=as.Date(paste(input$year,"-01-01", sep=''))& hur$Date <= as.Date(paste(input$year,"-12-31",sep=''))])})
    selectInput("names", "Names:", butt())
  })
  output$origin<-renderPlotly({
    input$goButton
    data1<-reactive({hur[hur$Date >= as.Date(paste(isolate(input$year),"-01-01", sep='')) & hur$Date <= as.Date(paste(isolate(input$year),"-12-31",sep='')) & hur$Name == toupper(isolate(input$names)),]})
    data2<-reactive({top_n(select(data1(), Latitude, Longitude),1)})
    dataf<-reactive({hur_start[hur_start$Latitude >= data2()$Latitude-1 & hur_start$Latitude<=data2()$Latitude+1 & hur_start$Longitude >= data2()$Longitude-1 & hur_start$Longitude<=data2()$Longitude+1,]})
    data3<-reactive({top_n(dataf(), 10)})
    full<-reactive({semi_join(hur, data3(), by = "ID")})
    plot_mapbox(mode = 'scattermapbox') %>%
      add_markers(
        data = full(), x = ~Longitude, y = ~Latitude, text=~paste('Name: ', Name, '<br>Max Wind:', Maximum.Wind, '<br>Date: ', Date), color=~ID,
        size = ~Maximum.Wind, alpha = 0.5) %>%
      layout(
        plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
        mapbox = list(style = 'dark',
                      zoom = 3.5,
                      center = list(lat = median(hur$Latitude),
                                    lon = median(hur$Longitude))),
        margin = list(l = 0, r = 0,
                      b = 0, t = 0,
                      pad = 0),
        showlegend=FALSE)
  })
  output$heat<-renderPlot({
    input$goButton
    heat1<-reactive({hur[hur$Date >= as.Date(paste(isolate(input$year),"-01-01", sep='')) & hur$Date <= as.Date(paste(isolate(input$year),"-12-31",sep='')) & hur$Name == toupper(isolate(input$names)),]})
    heat2<-reactive({top_n(select(heat1(), Latitude, Longitude),1)})
    heatf<-reactive({hur_start[hur_start$Latitude >= heat2()$Latitude-1 & hur_start$Latitude<=heat2()$Latitude+1 & hur_start$Longitude >= heat2()$Longitude-1 & hur_start$Longitude<=heat2()$Longitude+1,]})
    heat3<-reactive({top_n(heatf(), 10)})
    fullheat<-reactive({semi_join(hur, heat3(), by = "ID")})
    heat<-ggmap(atlantic_map, extent = "device") + geom_density2d(data = fullheat(), 
                                                                  aes(x = Longitude, y = Latitude), size = 0.3) + stat_density2d(data = fullheat(), 
                                                                                                                                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE)
    heat
  }, width = 800, height = 600)
  output$tracks<-renderPlotly({
    input$goButton
    data1<-reactive({hur$Date >= as.Date(paste(isolate(input$year),"-01-01", sep='')) & hur$Date <= as.Date(paste(isolate(input$year),"-12-31",sep='')) & hur$Name == toupper(isolate(input$names))})
    example2<-filter(hur, data1())
    ex<-filter(hur, Latitude >= (example2$Latitude-1) &
                 Latitude<=example2$Latitude+1 &
                 Longitude >= (example2$Longitude-1) &
                 Longitude<=example2$Longitude+1)
    ex<-top_n(arrange(count(ex,ID), desc(n)), 5)
    ex_join<-semi_join(hur,ex)
    p7 <- plot_mapbox(mode = 'scatterbox') %>%
      add_markers(
        data = ex_join, x = ~Longitude, y = ~Latitude, text=~paste('Name: ', Name, '<br>Max Wind:', Maximum.Wind, '<br>Date: ', Date), color=~ID,
        size = ~Maximum.Wind, alpha = 0.5) %>%
      layout(
        plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
        mapbox = list(style = 'dark',
                      zoom = 3.5,
                      center = list(lat = median(hur$Latitude),
                                    lon = median(hur$Longitude))),
        margin = list(l = 0, r = 0,
                      b = 0, t = 0,
                      pad = 0),
        showlegend=FALSE)
    p7
  })
  output$table<-renderTable({
    data1<-reactive({hur$Date >= as.Date(paste(isolate(input$year),"-01-01", sep='')) & hur$Date <= as.Date(paste(isolate(input$year),"-12-31",sep='')) & hur$Name == toupper(isolate(input$names))})
    example2<-filter(hur, data1())
    input$goButton
    ex<-filter(hur, Latitude >= (example2$Latitude-1) &
                 Latitude<=example2$Latitude+1 &
                 Longitude >= (example2$Longitude-1) &
                 Longitude<=example2$Longitude+1)
    counter<-data.frame(arrange(count(group_by(ex,ID,Name)), desc(n)))
    top_n(counter, 5, n)
  })
}

shinyApp(ui = ui, server = server)
