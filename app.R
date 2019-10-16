#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shinydashboard)
library(reshape2)
library(shinythemes)
library(DT)
library(ggplot2)
library(tools)
library(stringr)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
pdf(NULL)
earthquake <- read.csv("C:/Users/user/Desktop/MINI 1/R-shiny/project2/work_on_this/earthquakes.csv")

earthquake$depth_level<-"Medium"

earthquake$depth_level[earthquake$Depth<70]<-"Shallow"
earthquake$depth_level[earthquake$Depth>300]<-"Deep"


#application header and title
header<-dashboardHeader(title="Earthquake summary from 1995 to 2011")

#dashboard sidebar
sidebar<-dashboardSidebar(
    sidebarMenu(
        id="tabs",
        menuItem("Map",icon=icon("map-pin"),tabName = "map"),
        menuItem("Plot",icon=icon("bar-chart"),tabName="plot"),
        menuItem("Table",icon=icon("table"),tabName = "table",badgeLabel = "new",badgeColor="red"),

        #ipnuts
        selectInput(inputId = "year1",
                    label = "From Year:",
                    choices=c("1995","1996","1997","1998","1999","2000","2001","2002",
                              "2003","2004","2005","2006","2007","2008","2009","2010","2011")),
        selectInput(inputId = "year2",
                    label = "To Year:",
                    choices=c("1995","1996","1997","1998","1999","2000","2001","2002",
                              "2003","2004","2005","2006","2007","2008","2009","2010","2011")),
        checkboxInput("heat", "Heatmap", FALSE),
        checkboxInput("markers", "Markers", FALSE),
        actionButton(inputId = "write_csv", 
                     label = "Write CSV")
    )
)

#dashboard body
body<-dashboardBody(tabItems(
    #map page
    tabItem("map",
            fluidRow(
                tabBox(title="map showing earthquake depth level in selected years",
                       width=12,
                       tabPanel("Map",leafletOutput("mymap")))
                       
            )
            
    ),
            
    #plot page
    tabItem("plot",
            
            fluidRow(
                tabBox(title="histogram and boxplot",
                       width=12,
                       tabPanel("histogram",plotlyOutput("histogram_mag")),
                       tabPanel("boxplot",plotlyOutput("plot_box")))
            )
            
    ),
    
    #data table page
    tabItem("table",
            
            fluidPage(
                box(title="key information of earthquakes in selected years",
                    DT::dataTableOutput("table"),width=12)
            ))
)

)


ui<-dashboardPage(header,sidebar,body,skin="red")
# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
    
    output$mymap <- renderLeaflet({
        
        leaflet() %>%
            #add markers on OpenStreetMap
            addProviderTiles("Esri.WorldImagery",group="World Imagery") %>%
            addProviderTiles("Stamen.TonerLite",group="Toner Lite")%>%
            addLayersControl(
                baseGroups = c("World Imagery",  "Toner Lite"),
                overlayGroups = c("data","data2"),
                options = layersControlOptions(collapsed = FALSE)
            )
           
    })
    
    earthquakeinputs<-reactive({
        earthquake1<-subset(earthquake,Year>=as.numeric(input$year1) & Year <=as.numeric(input$year2))
        return (earthquake1)
    })
    observe({
        if(input$markers){
        data=earthquakeinputs()
        palette = colorFactor(palette = c("#d73027", "#fff49c","#1a9850"),data$depth_level)
    
        leafletProxy("mymap",data=data) %>%
      
            clearGroup("data") %>%
            removeControl("legend")%>%
            addCircleMarkers(lng =data$Longitude ,lat = data$Latitude,group="data", popup=paste("long:",data$Longitude,"lat:",data$Latitude,"depth:",data$Depth),color=~palette(depth_level),radius = 0.1)%>%
            addLegend(position = "topright" , pal = palette, values = data$depth_level, title = "Depth Level",layerId = "legend")
        
   } 
         else{leafletProxy("mymap",data=data) %>%clearGroup("data")%>%removeControl("legend")} 
            })
    
    
    observe({
        if(input$heat){
                     data=earthquakeinputs()
                     palette2=colorNumeric("inferno",data$Depth)
                     leafletProxy("mymap",data=data) %>%
                        
                         clearGroup("data2") %>%
                         
                         #problems about heatmap
                         addHeatmap(lng =data$Longitude ,lat = data$Latitude,radius=10,blur=10,group="data2", 
                                    intensity = log(data$Depth)/10)
        }
        else{leafletProxy("mymap",data=data) %>%clearHeatmap()}           
                 
    })
    output$histogram_mag<-renderPlotly({
        ggplotly(
            ggplot(earthquakeinputs(), aes(x = Magnitude)) + 
                geom_histogram(bins=10,color="navy",fill="yellow")+labs(x="magnitude",y="number of earthquakes",
                                                                        title=paste("Maginitude distribution of earthquakes from",input$year1,"to",input$year2))
        )
    })
    output$plot_box<-renderPlotly({
        ggplotly(
            ggplot(earthquakeinputs(), aes(x = factor(Year), y = Duration) )+ 
                geom_boxplot(color="purple")+labs(x="Year",y="duration in seconds",
                                                  title=paste("Duration distribution of earthquakes from",input$year1,"to",input$year2))
        )
    })
    
    output$table <- DT::renderDataTable(
        
        DT::datatable(data = earthquakeinputs()[,c(-2,-3,-9)], 
                      
                      
                      extensions='FixedHeader',
                      options=list(
                          autoWidth=TRUE,
                          fixedHeader=TRUE,
                          columnDefs = list(list(width = '100px', targets = c(1,2))),
                          pageLength=20
                          
                      ), 
                      
                      rownames = FALSE)
    )
    
    # Write selected data as csv ---------------------------------------
    observeEvent(eventExpr = input$write_csv, 
                 handlerExpr = {
                     filename <- paste0("Key Information of Earthquakes in ",input$year1,"-",input$year2, ".csv")
                     write.csv(earthquakeinputs()[,c(-2,-3,-9)], file = filename, row.names = FALSE) 
                 }
    )
}




# Run the application 
shinyApp(ui = ui, server = server)
