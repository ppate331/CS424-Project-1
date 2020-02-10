library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(rsconnect)

litt <- read.table(file = "litterati challenge-65.csv", sep = ",", header = TRUE)
litt$username <- as.character(litt$username)
litt$tags <- as.character(litt$tags)
litt$url <- as.character(litt$url)
litt$litterTimestamp <- as.character(litt$litterTimestamp)
litt <-litt[(litt$lat<42.5 & litt$lat>41.5),]
litt <-litt[(litt$lon > -89 & litt$lon < -83),]
litt$tags[litt$tags == ""] = "untagged"
litt3 <-litt
litt2<-litt

#code for plot of litter picked up for new tags
litt3$tags[(grepl(',', litt3$tags)) == TRUE] <- NA
tab2 <- table(a<-c(litt3$tags))
tagsrankings <- as.data.frame(tab2)
newtags <- head(tagsrankings[order(-tagsrankings$Freq),], 10)
names(newtags)[1] = "Tags"

#litter by date code for table graph
littymd <- litt2
littymd$litterTimestamp <- as.POSIXct(x = littymd$litterTimestamp, format = "%Y-%m-%d")
str(littymd)
gl <-table(a<-(littymd$litterTimestamp))
graphlitt <- as.data.frame(gl)
names(graphlitt)[1] = "Date"


#litter by hour code for table graph
litthour <- litt2
#litthour$litterTimestamp <- as.POSIXct(x = litthour$litterTimestamp, format = "%H:%M:%S")
hour = format(as.POSIXct(litthour$litterTimestamp,format="%Y-%m-%d %H:%M:%S"),"%H")

str(litthour)
glhour <-table(a<-(hour))
graphlitthour <- as.data.frame(glhour)
names(graphlitthour)[1] = "Hour"


#litter by day code for table graph
littday <- litt

weekday <- weekdays(as.Date(littday$litterTimestamp,"%Y-%m-%d"))
glday <-table(a<-(weekday))
graphlittday <- as.data.frame(glday)
names(graphlittday)[1] = "Day"



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    
    
  ),
  dashboardBody(
    fillRow(padding = 100),
    fluidRow(
      height = 200
    ),
    
    fluidRow(
      box(title = "Forest Park Area", status = "primary", solidHeader = TRUE, width = 8,leafletOutput("map", height = 400)
      ),
      box(title = "total litter", status = "primary", solidHeader = TRUE, width = 2,
          textOutput("number"),
          verbatimTextOutput("num")
          )
    ),
    
    
      fluidRow(
        
      box(title = "top 10 pickers", solidHeader = TRUE, width = 3,
            DT::dataTableOutput("userrankings", height= 200)
      ),
      box(title = "top 10 litter table", solidHeader = TRUE, width = 3,
          DT::dataTableOutput("newtags", height= 200)
          ),
      box(title = "top 10 litter graph", solidHeader = TRUE, width = 6,
          plotOutput("plotlitter", height= 400)
      )
      
    ),
    
    fluidRow(
      box(title = "litter by date table", solidHeader = TRUE, width = 3,
          DT::dataTableOutput("graphlitt", height= 200)
      ),      
      box(title = "litter by date graph", solidHeader = TRUE, width = 9,
          plotOutput("plotdaylitter", height= 400)
      )
      
    ),
    
    
  fluidRow(
    box(title = "litter by hour table", solidHeader = TRUE, width = 3,
        DT::dataTableOutput("graphlitthour", height= 200)
    ),      
    box(title = "litter by hour graph", solidHeader = TRUE, width = 9,
        plotOutput("plotgraphhour", height= 400)
    )    
  ),  
    
  fluidRow(
    box(title = "litter by day table", solidHeader = TRUE, width = 3,
        DT::dataTableOutput("graphlittday", height= 200)
    ),      
    box(title = "litter by day graph", solidHeader = TRUE, width = 9,
        plotOutput("plotlittday", height= 400)
    )    
  )   
    
  
  )
)


server <- function(input, output) {
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(data = litt, lng = ~lon, lat = ~lat, clusterOptions = markerClusterOptions())
      #setView(map,lat = )
    m
  }) 
  output$userrankings <- DT::renderDataTable({
    litt2 <- litt
    tab <- table(a<-c(litt2$username))
    userrankings <- as.data.frame(tab)
    #userrankings
    
    typeof(userrankings)
    
    userrankings <- head(userrankings[order(-userrankings$Freq),], 10)
    names(userrankings)[1] = "Names"
    userrankings
  })
  output$newtags <- DT::renderDataTable({
    litt3$tags[(grepl(',', litt3$tags)) == TRUE] <- NA
    tab2 <- table(a<-c(litt3$tags))
    tagsrankings <- as.data.frame(tab2)
    newtags <- head(tagsrankings[order(-tagsrankings$Freq),], 10)
    names(newtags)[1] = "Tags"
    newtags
  })
  output$plotlitter <- renderPlot({
    plotlitter <- ggplot(data=newtags, aes(x=Tags, y=Freq)) + 
      geom_bar(stat="identity", color="blue", fill="white")+
      geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)
    plotlitter
    
  })
  
  output$graphlitt <- DT::renderDataTable({
    littymd <- litt2
    littymd$litterTimestamp <- as.POSIXct(x = littymd$litterTimestamp, format = "%Y-%m-%d")
    str(littymd)
    gl <-table(a<-(littymd$litterTimestamp))
    graphlitt <- as.data.frame(gl)
    names(graphlitt)[1] = "Date"
    graphlitt
    
  })
  
  output$plotdaylitter <- renderPlot({
    plotdaylitter <- ggplot(data=graphlitt, aes(x=Date, y=Freq)) + 
      geom_bar(stat="identity", color="blue", fill="white")
    plotdaylitter
  })
  
  output$graphlitthour <- DT::renderDataTable({
    litthour <- litt2
    #litthour$litterTimestamp <- as.POSIXct(x = litthour$litterTimestamp, format = "%H:%M:%S")
    hour = format(as.POSIXct(litthour$litterTimestamp,format="%Y-%m-%d %H:%M:%S"),"%H")
    
    str(litthour)
    glhour <-table(a<-(hour))
    graphlitthour <- as.data.frame(glhour)
    names(graphlitthour)[1] = "Hour"
    graphlitthour
    
  })
  
  output$plotgraphhour <- renderPlot({
    plotgraphhour <- ggplot(data=graphlitthour, aes(x=Hour, y=Freq)) + 
      geom_bar(stat="identity", color="blue", fill="white")
    plotgraphhour
  }) 
  
  output$graphlittday <- DT::renderDataTable({
    littday <- litt
    
    weekday <- weekdays(as.Date(littday$litterTimestamp,"%Y-%m-%d"))
    glday <-table(a<-(weekday))
    graphlittday <- as.data.frame(glday)
    names(graphlittday)[1] = "Day"
    graphlittday
    
  })
  output$plotlittday <- renderPlot({
    plotlittday <- ggplot(data=graphlittday, aes(x=Day, y=Freq)) + 
      geom_bar(stat="identity", color="blue", fill="white")
    plotlittday
  })  
  
  output$num <- renderText({
    length(litt$tags)
  })
  
}

shinyApp(ui, server)

