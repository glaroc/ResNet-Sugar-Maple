#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(reshape2)
#library(sp)
#library(rgdal)
library(leaflet)
library(dplyr)
library(plotly)

#change level or timePeriod
level1 <- "10"
#pathIn <- "/home/glaroc/OneDrive/ResNet/datasets/Sugar Maple/"

#tabES <- read.csv(paste0(pathIn,"networks/tables/tabES_",level1,".csv"), stringsAsFactors = F)

#spolyHS <- sf::read_sf(paste0(pathIn,"GeospatialData/hydroBasins"), layer = paste0("hydroBasinQuebec",level1,"albersQclip2"))
#spolyHS_LL <- sf::st_transform(spolyHS, crs = 4326)

#MetList = names(tabES)[3:41]

#mainES <- merge(spolyHS_LL,tabES, by = c("HYBAS_ID"))
#saveRDS(mainES,'./www/mainES.rds')

mainES<-readRDS('./www/mainES.rds')

ES_options=names(mainES)[16:54]

ui <- fillPage(theme='style.css',
              tags$head(tags$script(src = "tableau.js")),
              fillRow(flex=c(3,1,1),
                fillCol(div(
                  class="left-header",div(class="logo",imageOutput("logo",height="90px")),div(class="dash-title","Services provided by sugar maple production in Quebec")),hover=""),
                height="7vh"
                ),
             fillRow(flex=c(2,8),id="main-row",
                fillCol(id="sidebar",
                  div(div(id="closebtn-div",a(href="javascript:void(0)", id="closebtn", onclick="closeOpenNav()",'<')),
                     div(class="blue-badge","Example dashboard for Sugar Maple production in Quebec. "),
                     div(class="left-controls",selectInput("year",
                                 label="Year",
                                 choices=c(2006,2016,"2006-2016"),
                                 selected=2016)),
                     div(class="left-controls",selectInput("var_es",
                                   label="Ecosystem service",
                                   choices=ES_options,
                                   selected = "Number_of_taps",
                                   selectize = FALSE))
                   )),
                fillCol(id="main",
                     tabsetPanel(type = "tabs",tabPanel("Map",leafletOutput("map",height="90vh",width="80vw")),
                                 tabPanel("Other",div("")))
                   ),
                   height="93vh"
                )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$logo <- renderImage({
    filename <- normalizePath(file.path('./www/resnet-logo-4x.png'))
    list(src = filename, height="60px", width="60px")
  }, deleteFile = FALSE)
  
  varES = reactive({
    input$var_es
  })


  leaflet_poly = reactive({
    if (input$year == '2006-2016'){
      y1 = mainES %>% filter(year=="2016") %>% select(c(var=input$var_es,id=HYBAS_ID))
      y2 = mainES %>% filter(year=="2006") %>% select(c(var=input$var_es,id=HYBAS_ID))
      y1$var = y2$var-y1$var
      return(y1)
    }else {
      return(mainES %>% filter(year==input$year) %>% select(c(var=input$var_es,id=HYBAS_ID)))
    }
  })

  #qpal  = reactive({
  #  
  #})

  pal <- reactive({
    tt<-leaflet_poly()[,1,drop=TRUE]
    if(input$year=="2006-2016"){
      qpal <- colorNumeric(colorRamp(c("#00dd00","#ffffff","#dd0000"), interpolate = "spline"),tt, n = 50)
    }else{
      qpal <- colorNumeric("Oranges",tt, n = 50)
    }
    cols <- qpal(tt)
    return(cols)
  })

  factop <- function(x) {
    ifelse(is.na(x$var) | x$var==0, 0, 0.7)
  }
    
  output$map <- renderLeaflet({
    leaflet(leaflet_poly()) %>%
    addTiles() %>% # Affichage du fond de carte
    addPolygons(color = "white", # couleur des limites des polygones
                weight = 0.4,
                smoothFactor = 0.5,
                fillColor = pal(), # couleur du remplissage des polygones
                fillOpacity = factop(leaflet_poly()),
                opacity = 1,
                layerId=~id,
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 4,
                                                    fillOpacity = 0.7,
                                                    bringToFront = TRUE))
    })
  

  dataModal <- function(basin){
    mainES %>% filter(HYBAS_ID == basin & (year==2006)) %>% sf:::st_drop_geometry() %>% select(varES()) -> this2006
    mainES %>% filter(HYBAS_ID == basin & (year==2016)) %>% sf:::st_drop_geometry() %>% select(varES()) -> this2016
      modalDialog(
      h3(paste0(basin,' - ',varES())),
        renderPlotly(
              plot_ly(
                 y = "2006",
                 x = t(this2006),
                 name = "2006",
                 type = "bar",
                 orientation = 'h'
             ) %>% add_trace(y = "2016", x=t(this2016),name = '2016') %>% layout(yaxis = list(size=4),xaxis = list(size=4), barmode = 'group', showlegend=FALSE)
          ), size = 'l'
      )
  }
  
  observe({
    event <- input$map_shape_click
    if(!is.null(event$id)){
      showModal(dataModal(event$id))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



              