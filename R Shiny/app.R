library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
#hur_start$Latitude<-(-1)*hur_start$Latitude
years<-c(1851:2015)
hur_start<-hur[!duplicated(hur$ID), ] #Dataset of only origin of systems.
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
             tabPanel("Closest Paths",
                     sidebarPanel(
                               tags$div(
                                 tags$p("Finding top 5 closest tracks.")),
                               tags$head(tags$style("#tracks{height:100vh !important;}")),
                               tableOutput("table"), width = 2),
                     mainPanel(plotlyOutput("tracks"), width = 10)
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
