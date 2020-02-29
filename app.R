################################################################################################
################################################################################################
#### Pedestrian Traffic in the city of Melbourne
#### Author : Carlos Yanez Santibanez
################################################################################################
# This shiny app shows the pedestrian traffic from the last hour, as recorded by sensors deployed 
# by the City of Melbourne (Australia).
# This data made publicly available by the City Council through their Open Data portal:
# https://data.melbourne.vic.gov.au
# Bootstrap CSS : https://bootswatch.com/sandstone/
################################################################################################

# Load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

#Download

dl <- "bootstrap.css"
download.file("https://bootswatch.com/4/sandstone/bootstrap.css", dl)

# Colour ranges for markers

breaks <- c(0,200,3000,10000,Inf)
labels <- c("gray","green","orange","red")
legend <-c("x<200","200<x<5,000","5,000<x<10,000","10,000")

# Coordinates of Melbourne's GPO building (the "centre" of town)
gpo_lat <- -37.8076
gpo_lng <- 144.9568

# Download and read list of sensors' names and coordinates
dl <- tempfile()
download.file("https://data.melbourne.vic.gov.au/api/views/h57g-5234/rows.csv?accessType=DOWNLOAD", dl)

sensors <- read.csv(dl)
sensors <- sensors %>% select(sensor_id,latitude,longitude,sensor_description) 

# Shiny UI's function
ui <- fluidPage(theme = "bootstrap.css",
  
  # App title ----
  titlePanel("Pedestrians in Melbourne CBD in the last hour"),
  uiOutput("source_link"),
  uiOutput("measurement_times"),
  actionButton("recalc", "Refresh",class = "btn-primary"),  
  
  tabsetPanel(type = "tabs",
              tabPanel("Map", leafletOutput("mymap")),
              tabPanel("Table", tableOutput('table'))
  )
  
)

server <- function(input, output) {
  
  #download traffic data
  
  dl <- tempfile()
  download.file("https://data.melbourne.vic.gov.au/api/views/d6mv-s43h/rows.csv?accessType=DOWNLOAD", dl)
  pedestrian <- read.csv(dl)
  
  rm(dl)
 
  # Get measurement times
  
  
  data_from <- min(pedestrian %>% mutate(datetime=as_datetime(parse_date_time(as.character(date_time),
                                                              '%y:%m:%d %I:%M:%S %p',
                                                              tz="Australia/Melbourne"),
                                                              tz="Australia/Melbourne")) %>% pull(datetime))
  data_until <- max(pedestrian %>% mutate(datetime=as_datetime(parse_date_time(as.character(date_time),
                                                                                '%y:%m:%d %I:%M:%S %p',
                                                                                tz="Australia/Melbourne"),
                                                                tz="Australia/Melbourne")) %>% pull(datetime))
  
  # Add coordinates and colour
  
  pedestrian <- pedestrian %>% group_by(sensor_id) %>% 
    summarise(total=sum(total_of_directions)) %>%
    ungroup() %>% left_join(sensors,by="sensor_id") %>% 
    mutate(group=cut(total,breaks = breaks,labels = labels,include.lowest = TRUE),
           label=paste(sensor_description,total,sep = ": "))
  
  icons <- awesomeIcons(icon = "whatever",
                        iconColor = "black",
                        library = "ion",
                        markerColor = pedestrian$group)
  # Refer Source and last update
  
  url <- a("City of Melbourne's Open Data Portal",
           href="https://data.melbourne.vic.gov.au/Transport/Pedestrian-Counting-System-Past-Hour-counts-per-mi/d6mv-s43h")
  
  
  output$source_link <- renderUI({
    tagList("Source: ", url)
  })
  # Generate from and to Reference
  
  output$measurement_times <- renderUI({
    tagList("Measured between: ", data_from," and ",data_until)
  })
  
  # Output Table
  output$table <- renderTable(pedestrian%>%select(sensor_id,station=sensor_description,latitude,longitude,total))
  
  
  # Generate Map
  output$mymap <- renderLeaflet({
   leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(lng=gpo_lng,lat=gpo_lat, zoom = 14) %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addAwesomeMarkers(data = pedestrian,~longitude, ~latitude, icon = icons, label = ~label) 

  })
  
}

shinyApp(ui = ui, server = server) 