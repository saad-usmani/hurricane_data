require("data.table")
require("ggplot2")
require('lubridate')
require('maps')
require('sp')
require('ggmap')
require('plotly')
require('dplyr')
hur <- fread(file.path("C:/Users/susmani/Documents/Year 5/Courses/Data Munging/GroupProject2/hurricane.csv"),na.strings = c("PrivacySuppressed", "NULL"))
hur<-data.frame(hur)
ggplot(hur, aes(hur$Date[hur$ID=="AL031861"], hur$`Maximum Wind`[hur$ID=="AL031861"]))
ggplot(hur[hur$ID=="AL031861",], aes(Date,Maximum.Wind))+geom_point()
ggplot(hur, aes(Date,Maximum.Wind))+geom_point() + ylim(0,200)
as.Date(hur$Date, "%y %m %d")
hur$Date<-ymd(hur$Date)

ggplot(hur, aes(factor(Status)))+geom_bar()
gp<-qplot(x=Longitude, y=Latitude, data=hur[hur$ID=="AL122005",], color=Status)
ggplot(hur[hur$Date >= "2005-01-01" & hur$Date <= "2006-02-01",], aes(Date,Maximum.Wind), color=Maximum.Wind)+geom_point()

mp <- NULL
mapWorld <- borders("florida", colour="gray50", fill="gray50") # create a layer of borders
mp<-gp+mapWorld

mp <- mp+ geom_point(aes(x=Longitude, y=Latitude) ,color="blue", size=3) 

hur$Latitude<-as.numeric(unlist(strsplit(hur$Latitude, split='N', fixed=TRUE)))
hur$Longitude<-as.numeric(unlist(strsplit(hur$Longitude, split='W', fixed=TRUE)))
hur$Longitude<-(-1)*hur$Longitude

mapgilbert <- get_map(location=c(-65.280479, 24.923566), zoom = 4,
                      maptype = "satellite", scale = 2)

gg<-ggmap(mapgilbert)+geom_point(data = hur[hur$ID=="AL122005",], aes(x = Longitude, y = Latitude, color=Maximum.Wind, alpha = 1), size = 5, shape = 20)+scale_colour_gradient(low='green', high='red')

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

p <- plot_geo(hur[hur$Date >= "2005-01-01" & hur$Date <= "2006-02-01",], lat = ~Latitude, lon = ~Longitude) %>%
  add_markers(text = ~paste(Maximum.Wind, Name, Minimum.Pressure, sep = "<br />"), color = ~Maximum.Wind, symbol = I("square"), size = I(8), hoverinfo = "text"
  ) %>%
  colorbar(title = "Incoming flights<br />February 2011") %>%
  layout(
    title = 'Most trafficked US airports<br />(Hover for airport)', geo = g
  )
ggplotly(ggmap()+geom_point(data = hur[hur$ID=="AL122005",], aes(x = Longitude, y = Latitude, color=Maximum.Wind, alpha = 1), size = 5, shape = 20)+scale_colour_gradient(low='green', high='red'))

dat <- map_data("world", "canada") %>% group_by(group)
p <- plot_mapbox(hur[hur$Date >= "2005-01-01" & hur$Date <= "2006-02-01",], x = ~Longitude, y = ~Latitude) %>%
  add_paths(size = I(2)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75) %>%
  layout(mapbox = list(zoom = 0,
                       center = list(lat = ~median(lat),
                                     lon = ~median(long))
  ))

pk.eyJ1Ijoic3VzbWFuaSIsImEiOiJjamFhZnJlb3YwczdmMzJxaXlmcHJ0ZGZ2In0.vdCC--CzL7cM-XsG8yCrFw
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoic3VzbWFuaSIsImEiOiJjamFhZnJlb3YwczdmMzJxaXlmcHJ0ZGZ2In0.vdCC--CzL7cM-XsG8yCrFw')

p <- hur[hur$Date >= "2005-01-01" & hur$Date <= "2006-02-01",]  %>%
  plot_mapbox(lat = ~Latitude, lon = ~Longitude,
              mode = 'scattermapbox', color = I('red'), size = ~Maximum.Wind, hovertext=~paste("Max Wind: ", Maximum.Wind, '<br>Name:', Name))%>%
  layout(title = 'Meteorites by Class',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))

#2005 Interactive Graph

p <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = hur[hur$Date >= "2005-01-01" & hur$Date <= "2006-02-01",], x = ~Longitude, y = ~Latitude, text=~paste('Name: ', Name, '<br>Max Wind:', Maximum.Wind, '<br>Date: ', Date), split=~Name,
    size = ~Maximum.Wind, alpha = 0.5) %>%
  layout(
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(style = 'dark',
                  zoom = 1.5,
                  center = list(lat = median(hur$Latitude),
                                lon = median(hur$Longitude))),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=FALSE)


# Switch between September and October Major Hurricanes
p2 <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = hur[format.Date(hur$Date, "%m")=="09" & hur$Maximum.Wind>=115,], x = ~Longitude, y = ~Latitude, text=~paste('Name: ', Name, '<br>Max Wind:', Maximum.Wind, '<br>Date: ', Date), color=~Name,
    size = ~Maximum.Wind, alpha = 0.5) %>%
  layout(
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(style = 'dark',
                  zoom = 1.5,
                  center = list(lat = median(hur$Latitude),
                                lon = median(hur$Longitude))),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=FALSE)

p3 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = hur$Maximum.Wind[format.Date(hur$Date, "%m")=="09"]) %>%
  add_histogram(x = hur$Maximum.Wind[format.Date(hur$Date, "%m")=="06"]) %>%
  layout(barmode = "overlay")

p4<-plot_ly(x = hur$Maximum.Wind[format.Date(hur$Date, "%m")=="09"], type = "histogram")

max_winds<-data.frame(hur) %>%
  group_by(ID) %>%
  dplyr::summarise(Max.Winds = max(Maximum.Wind))

full_join(max_winds, hur, by = c('ID', 'Maximum.Wind'))

#Max Winds for Each Tropical Cyclone
max_winds<-data.frame(hur) %>%
  group_by(ID) %>%
  filter(Maximum.Wind == max(Maximum.Wind)) %>%
  filter(row_number() <= 1) 

#First Location of Maximum Wind for Major Hurricane
plot_mapbox(mode = 'scattermapbox') %>%
       add_markers(
             data = max_winds[max_winds$Maximum.Wind>115,], x = ~Longitude, y = ~Latitude, text=~paste('Name: ', Name, '<br>Max Wind:', Maximum.Wind, '<br>Date: ', Date), split=~Name,
             size = ~Maximum.Wind, alpha = 0.5) %>%
       layout(
             plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
             mapbox = list(style = 'dark',zoom = 1.5, center = list(lat = median(hur$Latitude), lon = median(hur$Longitude)))
             margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
             showlegend=FALSE)

p5<-plot_ly(x = format.Date(max_winds$Date, "%m"), type = "histogram")
p6<-plot_ly(x = format.Date(max_winds$Date, "%Y"), type = "histogram")
p7<-plot_ly(x = format.Date(max_winds$Date, "%Y%m%d"), type = "histogram")


