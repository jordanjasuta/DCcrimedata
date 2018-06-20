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
library(ggplot2)

#load cleaned file
crime <- read.csv('C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Crime_new.csv') 

#library(dplyr)
crime$start_day <- as.numeric(crime$start_day)
crime$start_month <- as.numeric(crime$start_month)
crime$start_year <- as.numeric(crime$start_year)
crime$dow <- factor(crime$dow, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
crime$TIME_OF_DAY <- factor(crime$TIME_OF_DAY, levels= c("morning", "afternoon", "night"))
crime$quarter <- factor(crime$quarter, levels= c("NE", "SE", "SW", "NW", "unclear"))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   fluidRow(column(12,h2("Analysis: Crime in DC, 2017", align = "center"))),

   # Sidebar filters to look at specific offenses and months 
   sidebarLayout(
      sidebarPanel(
        sliderInput("range",
                    "Select months of interest: ",
                    min = 1,
                    max = 12,
                    value = c(1,12), step=1),
                     
        checkboxGroupInput("Offense", "Select offense of interest:",
                            levels(crime$OFFENSE), "ARSON"), 
        actionLink("selectall","Select All"),
        fluidRow(column(12,h6("All data used in this analysis was downloaded with permission from http://opendata.dc.gov/datasets/6af5cb8dc38e4\nbcbac8168b27ee104aa_38/geoservice on December 15, 2017. According to the source, 'The dataset contains a subset of locations and attributes of incidents reported in the ASAP (Analytical Services Application) crime report database by the District of Columbia Metropolitan Police Department (MPD).' ", align = "center")))
      ),
      
      # Show each plot on a new tab
      mainPanel(
        tabsetPanel(
          tabPanel("Map by crime", fluidRow(class = "myRow1",column(12,h4("Select crimes reported in the District of Columbia", align = "center"),div(style = "height:400px;",leafletOutput('sm1', width='100%')))),
                   fluidRow(class = "myRow2", column(8,div(style = "height:250px;", plotOutput('crimetype'))), column(4,div(style="height:250px;", plotOutput('crimequarter')))
          )),
          tabPanel("Map by time of day", fluidRow(class = "myRow3",column(12, h4("Time of day of select crimes reported in DC", align = "center"),div(style = "height:400px;",leafletOutput('sm2', width='100%')))),
                   fluidRow(class = "myRow4", column(6,div(style = "height:250px;", plotOutput('byMonth', width="200%")))
          )),
          tabPanel("Day of Week", plotOutput('byDOW')),
          #tabPanel("Quarter", plotOutput('crimequarter')),
          tabPanel("Time of Day", plotOutput('byHR'))
          
          ))
        )
        

#################
)
###############################

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"Offense","Select offense of interest:",
                               levels(crime$OFFENSE), "ARSON")
    }
    else
    {
      updateCheckboxGroupInput(session,"Offense","Select offense of interest:", choices=levels(crime$OFFENSE),selected=levels(crime$OFFENSE))
    }
  })
  
  ldData <- reactive({
    varRangeGet <- range(input$range)
    subset(crime, OFFENSE %in% input$Offense & start_month <= varRangeGet[[2]] & start_month >= varRangeGet[[1]])
  })
  
    output$sm1 <- renderLeaflet({
    d1 <- ldData()
    OffenseIcons <- icons(
      iconUrl = ifelse(d1$OFFENSE == 'ARSON',
                       "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Arson2.png",
                       ifelse(d1$OFFENSE =='ASSAULT W/DANGEROUS WEAPON', 
                              "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\DangerousWeapon2.png",
                              ifelse(d1$OFFENSE =='BURGLARY', "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Burglary2.png",
                                     ifelse(d1$OFFENSE =='HOMICIDE', 
                                            "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Homicide2.png",
                                            ifelse(d1$OFFENSE =='THEFT/OTHER', 
                                                   "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Theft2.png",
                                                   ifelse(d1$OFFENSE =='MOTOR VEHICLE THEFT',
                                                          "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\MVTheft2.png",
                                                          ifelse(d1$OFFENSE =='ROBBERY',
                                                                 "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Robbery2.png",
                                                                 ifelse(d1$OFFENSE == 'SEX ABUSE',
                                                                        "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\SexAbuse2.png",
                                                                        "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Autotheft2.png")))))))
                       
      ),
      iconWidth = 13, iconHeight = 13,
      iconAnchorX = 4, iconAnchorY = 20
    )
    leaflet(data=d1) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addMarkers(data = d1, lng = d1$X, lat = d1$Y, icon=OffenseIcons, label = as.character(d1$OFFENSE) )
    })

    output$crimetype <- renderPlot({
      varRangeGet <- range(input$range)
      data <- subset(crime, start_month <= varRangeGet[[2]] & start_month >= varRangeGet[[1]])
      p <- ggplot(data, aes(x=OFFENSE))  + 
        geom_bar(data=data, stat="count") + theme_classic() + 
        ggtitle("Instances reported by type of crime") + xlab("Type of offense") + 
        #theme(axis.text.x = element_text(size=14, angle=45)) +   
        scale_x_discrete(labels=c("ARSON" = "Arson", "ASSAULT W/DANGEROUS WEAPON" = "Assault w/\ndangerous\nweapon", "BURGLARY"="Burglary", "HOMICIDE"="Homicide", "MOTOR VEHICLE THEFT"="Motor\nvehicle\ntheft", "ROBBERY"="Robbery", "SEX ABUSE"="Sex\nabuse", "THEFT F/AUTO"="Auto\ntheft", "THEFT/OTHER" = "Other\ntheft")) + 
        theme(plot.title = element_text(face='bold', hjust=.5))
      print(p)
    }, height = 250)
    
    
    output$crimequarter <- renderPlot({
      varRangeGet <- range(input$range)
      data <- subset(crime, OFFENSE %in% input$Offense & start_month <= varRangeGet[[2]] & start_month >= varRangeGet[[1]])
      p<- ggplot(data, aes(x=factor(1), fill=quarter))+
        geom_bar(width = 1) +  coord_polar("y") + 
        scale_fill_manual(values=c("#E69F00", "#56B4E9", "#1bbc09", "#bc0918", "#999999"))+xlab("")+ylab("")+ 
        theme_classic()+ggtitle("Crimes committed by\ngeographic quarter")+ 
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank())+
        theme(plot.title = element_text(face='bold', hjust=.5))
      print(p)
    }, height = 250)
    
    
    output$sm2 <- renderLeaflet({
      d1 <- ldData()
      TimeIcons <- icons(
        iconUrl = ifelse(d1$TIME_OF_DAY == 'night',
                         "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Icons\\Autotheft2.png",
                         ifelse(d1$TIME_OF_DAY =='morning', 
                                "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Icons\\Theft2.png",
                                "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Icons\\Burglary2.png")),
        iconWidth = 13, iconHeight = 13,
        iconAnchorX = 4, iconAnchorY = 20
      )
      
      
      leaflet(data=d1) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addMarkers(data = d1, lng = d1$X, lat = d1$Y, icon=TimeIcons, label = as.character(d1$TIME_OF_DAY) )
    })
    
        
    output$byMonth <- renderPlot({
      data <- subset(crime, OFFENSE %in% input$Offense)
      p <- ggplot(data, aes(x=start_month))  + 
        geom_bar(data=data, stat="count", aes(fill = TIME_OF_DAY)) + theme_classic() + 
        ggtitle("Instances reported by Month")+xlab("Month") + 
        theme(plot.title = element_text(face='bold', hjust=.5))
      print(p)
    }, height = 250)
    
    output$byDOW <- renderPlot({  
      varRangeGet <- range(input$range)
      data <- subset(crime, OFFENSE %in% input$Offense & start_month <= varRangeGet[[2]] & start_month >= varRangeGet[[1]])
      q <- ggplot(data, aes(x=dow))  + 
        geom_bar(data=data, stat="count", aes(fill = TIME_OF_DAY)) + theme_classic() + 
        ggtitle("Instances reported by Day of the Week")+xlab("Day of the Week")+ 
        theme(plot.title = element_text(face='bold', hjust=.5))
      print(q)
    }, height=500)
    
    
    output$byHR = renderPlot({
      varRangeGet <- range(input$range)
      data <- subset(crime, OFFENSE %in% input$Offense & start_month <= varRangeGet[[2]] & start_month >= varRangeGet[[1]])
      #data <- subset(crime, OFFENSE %in% input$Offense & start_month %in% input$month)
      s = ggplot(data, aes(x=HOUR))  +  
        geom_bar(data=data, stat="count", aes(fill = TIME_OF_DAY)) + theme_classic()+ 
        ggtitle("Instances reported by Hour of the Day")+xlab("Time of day (24 hours)")+ 
        theme(plot.title = element_text(face='bold', hjust=.5))
      print(s)
    })  
    
    
    
  

   
}
# Run the application 
shinyApp(ui = ui, server = server)

