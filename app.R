if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(dashboardthemes)) install.packages("dashboardthemes")
if(!require(leaflet)) install.packages("leaflet")
if(!require(leafpop)) install.packages("leafpop")
if(!require(collapsibleTree)) install.packages("collapsibleTree")
library(ggplot2)
library(shiny)
library(dplyr)
library(readr)
library(shinythemes)
library(rlist)
library(plotly)
library(shinyWidgets)
library(scales)
library(DT)
library(stringr)
library(viridis)
library(CGPfunctions)

#====data Processing====

# specifying the path
path <- 'data_preprocessed.csv'
# reading contents of csv file
data <- read.csv(path,sep=",")
#delete the blank in the column ISO3
data$ISO3=str_replace(data$ISO3," ","")
# convert to date and time
data$date_end <- strptime(data$date_end, format = "%Y-%m-%d %H:%M:%S")
data$date_start <- strptime(data$date_start, format = "%Y-%m-%d %H:%M:%S")
#calculate duration
data$duration <- as.numeric(difftime(data$date_end , data$date_start, units = "days"))
#duration of the conflict

#number of Victims and duration preprocess 
distinct_df = data %>% distinct(region)
dfregion = data %>% group_by(year,region) %>% summarise(Victims = sum(death), duration = sum(duration), conflicts = n_distinct(conflict_name))

dfcountry = data %>% group_by(year,country) %>% summarise(Victims = sum(death) , duration = sum(duration), conflicts = n_distinct(conflict_name))
colnames(dfcountry) <- c('year','region','Victims', 'duration', 'conflicts')
dfregions= bind_rows(dfregion,dfcountry)
distinct_countries = dfregions %>% distinct(region)
#region <- c("Asia","Americas","Europe" ,"Middle East","Africa")
region <- sort(as.character(unique(data[,c("region")])))
#number of conflict
dfConfliReg = data %>% group_by(year,region) %>% summarise(conflicts = n_distinct(conflict_name))
dfConfliCountry = data %>% group_by(year,country) %>% summarise(conflicts = n_distinct(conflict_name))
colnames(dfConfliCountry) <- c('year','region','conflicts')
dfconfli = bind_rows(dfConfliReg,dfConfliCountry)


dfW = data %>% group_by(year) %>% summarise(conflicts = n_distinct(conflict_name),Victims = sum(death))

#type of violence
dfViolenceReg = data %>% group_by(year,region,type_of_violence) %>% summarise(Victims = sum(death),conflicts = n_distinct(conflict_name))
dfViolenceCountry = data %>% group_by(year,country,type_of_violence) %>% summarise(Victims = sum(death),conflicts = n_distinct(conflict_name))
colnames(dfViolenceCountry) <- c('year','region','type_of_violence','Victims','conflicts')
dfviolence = bind_rows(dfViolenceReg,dfViolenceCountry)

# data for the table
dt_u = data %>% group_by(conflict_name,side_a,side_b,region, country)%>% summarise(Start_date = min(year),Active = max(active_year),Victims = sum(death),duration=max(year)- min(year))
dt_u$Active <- ifelse(dt_u$Active=="0", "Non-active", "Active")
colnames(dt_u) <- c("Conflict","Side A","Side B" ,"Region","Country","Start Date","Active conflict","Number of victims","Duration in Year")

#Dataset with conflict and Victims for the map
dfmap = data %>% group_by(year,country,ISO3) %>% summarise(Victims = sum(death), conflicts = n_distinct(conflict_name))
# using long and lat
data$longitude<- as.numeric(data$longitude)
data$latitude<- as.numeric(data$latitude)
dfmap_detail = data %>% filter(year>=2010)%>% group_by(year, country, latitude, longitude, type_of_violence) %>% summarise(Victims = sum(death), conflicts = n_distinct(conflict_name))
#overview
dfoverview = data %>% group_by( region, type_of_violence) %>% summarise(Victims = sum(death))
dfdyadregion  = data %>% group_by(year,region, dyad_name) %>% summarise(Victims = sum(death))
#dyad
dfdyad  = data %>% group_by(year,region, country,  dyad_name) %>% summarise(Victims = sum(death))

#summary 

dfsum  = data %>% group_by(year,region, country,  dyad_name, active_year) %>% summarise(Victims = sum(death), conflicts = n_distinct(conflict_name))
number_victims = sum(dfsum$Victims)
cum_victims_by_country = dfsum %>% group_by(country ) %>% summarise(Victims = sum(Victims))
dyad_sum = dfsum %>% group_by(dyad_name) %>% summarise(Victims = sum(Victims))
dyad_most_affected =dyad_sum[order(-dyad_sum$Victims),]

#overview fatality 
fatality = data<- data %>% filter(year %in% c(1989,1990, 2020)) %>% group_by(year,region) %>% summarise(Victims = sum(death)) 
fatality $year <-  as.character(fatality $year)


######################## Dashboard #################################
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("home")),
      menuItem("Regional view", tabName = "Regional_view", icon = icon("thumbtack")),
      menuItem("dyad tree", tabName = "tree", icon = icon("tree")),
      menuItem("Tabular search", tabName = "tabular_search", icon = icon("table")),
      menuItem("Map view", tabName = "map_view", icon = icon("map-marked")),
      menuItem("About this project", tabName = "about", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes( theme = "onenote"),
    
    tabItems(
      tabItem("Overview",
             
                
                  
                  fluidRow( 
                    column(width=5, valueBoxOutput("number_victims", width =12),
                    valueBoxOutput("dyad_most_affected_name", width = 12),
                    column(width=12, plotlyOutput('linechart1W')),),
                  
                 column(plotOutput ('lineoverview',height=800 ), width=5)),
                  
               
                 ),
             

      tabItem("Regional_view",
              
                fluidPage(
                  #title and description
                  fluidRow(
                    #Row with Two Bar charts            
                    fluidRow(
                      column(3,
                             sidebarPanel(width=12,style = "background-color:   #fef9e7 ;",
                                          # Year Slider
                                          sliderInput("years", " Years: ",
                                                      min = 1989,max = 2020,
                                                      value = c(1989,2020),
                                                      sep = "",
                                          ),
                                          #region or continent selection
                                          multiInput(
                                            inputId = "region",
                                            label = " Search and select different region:", 
                                            choices = unique(distinct_countries$region),
                                            selected = unique(distinct_df$region),
                                            choiceNames = unique(distinct_countries$region),
                                            choiceValues = unique(distinct_countries$region)
                                          )
                             )
                      ),
                      column(9,
                             tabsetPanel(
                               tabPanel("Total victims and conflicts",
                                        box(width=9,plotlyOutput('barchart1')),
                                        box(width =9, plotlyOutput('barchart2')),
                                        br("  "),
                                        tags$ul(
                                          tags$li(strong("Non-state conflict"),": use of armed force between organized groups, neither of which is the government of a state"), 
                                          tags$li(strong("State based conflict"),": use of armed force between government"), 
                                          tags$li(strong("One sided conflict"),": The deliberate use of armed force by the government of a state or by a formally organised group against civilians ")
                                        )
                               ),
                               
                               tabPanel("Temporal Evolution",
                                        column(12,plotlyOutput('linechart1')),
                                        column(12,plotlyOutput('linechart2'))),
                               tabPanel("Correlation",
                                        column(10, plotlyOutput("scatter"))
                                        )
                              
                                        
                             )
                      )
                    )
                  )
                )
      ),
      
      
              
      tabItem("map_view",
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                
                 leafletOutput('map'),
                
              absolutePanel( top = 80, right = 50, cursor = "move", 
                sliderInput("years3", " Years: ",
                            min = 2010,max = 2020,
                            value = c(2010,2020),
                            sep = ""), draggable = TRUE
                  )
              
              
              
                  ),
      tabItem("tabular_search",  
               
               fluidRow(
                 column(11,
                        h3("Search a conflict"),
                        p("Here you can search for a particular conflict and also export the data. "),
                        
                        )
               ),
               fluidRow(
                 column(12,
                        DT::dataTableOutput("mytable")
                 )
               )
      ),
      
      tabItem("tree", 
              fluidRow(
              # collapsible species tree section
              includeMarkdown("tree.md"),
              column(2, selectInput("region2","Select a region:", region)),
              collapsibleTreeOutput('tree', height='600px'))
              
      ),
      
      tabItem("about",
              
              includeMarkdown("about.md")
              
              
               )
  )
)
)

server <- function(input, output) { 

  # Years
  yearValues <- reactive({
    years = input$years
    first = years[1]
    second = years[2]
    c(first:second)
    
  })
  # years2
  yearValues2 <- reactive({
    years2 = input$years2
    first = years2[1]
    second = years2[2]
    c(first:second)
    
  })
  # region
  regionValues <- reactive({
    region = input$region
    first = region[1]
    second = region[2]
    three = region[3]
    fourth = region[4]
    five = region[5]
    c(first)
    print(region)
  })
  # region2
  regionValues2 <- reactive({
    region = input$region
    first = region[1]
    second = region[2]
    three = region[3]
    fourth = region[4]
    five = region[5]
    c(first)
    print(region)
  })

#======== Charts with ggplot and plotly ========

# line chart "Number of Victims" worldwide 
output$linechart1W <- renderPlotly({
  p <- ggplot(dfW) +
    aes(x = year, y = Victims) +
    scale_y_continuous(labels = label_number()) +
    geom_line(size = 1, color='#4C237E') +
    theme(plot.title = element_text(size = 8, face="bold"), panel.background = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.y = element_line(color = "gray"), panel.grid.major.x = element_line(color = "gray"),plot.background = element_blank(), 
          axis.line.y = element_line(size=1), axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_text(size = 12, face = "bold"))+
    scale_color_hue(direction = 1) +
    labs(x = "Year", y = "Victims", title = "Worldwide victims and conflicts evolution")

  # line chart "Number of conflicts" worldwide
  p2 <- ggplot(dfW) +
    aes(x = year, y = conflicts) +
    scale_y_continuous(labels = label_number()) +
    geom_line(size = 1, color='#4C237E') +
    theme(plot.title =  element_text(size = 8), panel.background = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.grid.major.y = element_line(color = "gray"), panel.grid.major.x = element_line(color = "gray"),plot.background = element_blank(), 
          axis.line.y = element_line(size=1), axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_text(size = 12, face = "bold"))+
    scale_color_hue(direction = 1) +
    labs(x = "Year", y = "Conflicts")

  fig1<-ggplotly(p)
  fig2<-ggplotly(p2)
  subplot(fig1, fig2, nrows=2, shareX =TRUE, titleX=TRUE, titleY = TRUE)
})


###### tab2#############3

output$barchart1 <- renderPlotly({
  
  p <- ggplot(dfviolence %>% filter(year %in% yearValues() & region %in% regionValues() )%>% group_by(region,type_of_violence)   %>% summarise(Victims = sum(Victims)), aes(x= reorder(region, Victims),y=Victims,fill= type_of_violence)) + 
    scale_y_continuous(labels = label_number())+
    geom_bar(stat="identity", position = "dodge", color="black")+
    theme(plot.title = element_text(size = 12), axis.text= element_text(size = 12, face = "bold"),axis.text.x = element_text(angle = 25),
          panel.background = element_blank(),plot.background = element_blank(),legend.title = element_text(size=8), axis.title.x = element_blank(), axis.line = element_line(),panel.grid.major.y = element_line(color = "gray"))+
    labs(title = "Total number of victims per region")
 p+ scale_fill_viridis(discrete=TRUE)
  
})
#barchart  Total number of conflicts per region
output$barchart2 <- renderPlotly({
  
  p <- ggplot(dfviolence %>% filter(year %in% yearValues() & region %in% regionValues() )%>% group_by(region,type_of_violence)   %>% summarise(Conflicts = sum(conflicts)), aes(x=reorder(region, Conflicts),y=Conflicts, fill=type_of_violence))  +
    scale_y_continuous(labels = label_number())+
    geom_col(position = "dodge",stat="identity", color="black")+
    theme(plot.title = element_text(size = 12),legend.title = element_text(size=8),legend.text = element_text(size=8),axis.text= element_text(size = 12, face = "bold"),
          panel.background = element_blank(),plot.background = element_blank(),axis.text.x = element_text(angle = 25), axis.title.x = element_blank(), axis.line = element_line(), panel.grid.major.y = element_line(color = "gray"))+
    labs(title = "Total number of conflicts per region")
  p+ scale_fill_viridis(discrete=TRUE)
})
# line chart "Number of Victims"
output$linechart1 <- renderPlotly({
  p <- ggplot(dfregions %>% filter(year %in% yearValues() & region %in% regionValues() ) ) +
    aes(x = year, y = Victims, colour = region) +
    geom_line(size = 1) +
    theme(plot.title = element_text(size = 12),axis.text=element_text(size=8),legend.title = element_text(size=8),legend.text = element_text(size=8), 
          panel.background = element_blank(),plot.background = element_blank(), axis.line = element_line())+
    scale_y_continuous(labels = label_number()) +
    scale_color_hue(direction = 1) +
    labs(x = "Year", y = "Victims", title = "Evolution of the number of victims", color = "Regions")
  p+ scale_color_viridis(discrete=TRUE)
})

# line chart "Number of conflicts" 
output$linechart2 <- renderPlotly({
  p <- ggplot(dfconfli %>% filter(year %in% yearValues() & region %in% regionValues() ) ) +
    aes(x = year, y = conflicts, colour = region) +
    scale_y_continuous(labels = label_number()) +
    geom_line(size = 1) +
    theme(plot.title = element_text(size = 12),axis.text=element_text(size=8),legend.title = element_text(size=9),legend.text = element_text(size=8),
          panel.background = element_blank(),plot.background = element_blank(), axis.line = element_line())+
    scale_color_hue(direction = 1) +
    labs(x = "Year", y = "Conflicts", title = "Evolution of the number of conflicts", color = "Regions")
  p+ scale_color_viridis(discrete=TRUE)
  
})

# scatter plot 
output$scatter <- renderPlotly({
  p <- ggplot(dfregions %>% filter(year %in% yearValues() & region %in% regionValues() ) ) +
    aes(x = Victims, y = duration, color=region, size=conflicts ) +
    scale_y_continuous(labels = label_number()) +
    geom_point(alpha=0.5)+xlim(0,150000)+
    scale_size(range = c(.1, 7), name="Number of conflicts")
    theme(plot.title = element_text(size = 12), axis.text=element_text(size=12), legend.title = element_text(size=9), legend.text = element_text(size=9), 
          panel.background = element_blank(), plot.background = element_blank(), axis.line = element_line())+
    labs(x = "Conflict duration (days)", y = "Number of victims", title="Correlation victims/duration" ,color = "Regions")
    p+ scale_color_viridis(discrete=TRUE)
})

# create the leaflet map  
pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("one-sided violence", "non-state conflict", "state-based conflict"))
output$map <- renderLeaflet({
  leaflet(dfmap_detail) %>% 
    
    addTiles() %>%
    addCircleMarkers(lat =  ~latitude, lng =~longitude,
                     radius = 1, popup = ~leafpop::popupTable(dfmap_detail,
                                                             zcol = c("Victims", "conflicts", "type_of_violence"),
                                                             row.numbers = FALSE, feature.id = FALSE), 
                     color = ~pal(type_of_violence),
                     stroke = TRUE, fillOpacity = 0.8)%>%
    
    addLegend(pal=pal, values=dfmap_detail$type_of_violence,opacity=1, na.label = "Not Available",position = "bottomright", title="Type of conflict")%>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="ME",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
})
filtered <- reactive({
  dfmap_detail[dfmap_detail$year>= input$years3[1] & dfmap_detail$year<=input$years3[2], ]
})

observe(leafletProxy("map", data=filtered()) %>%
          clearMarkers()%>%
          addCircleMarkers(lat =  ~latitude, lng =~longitude,
                           radius = 1, popup = ~leafpop::popupTable(dfmap_detail,
                                                                    zcol = c("Victims", "conflicts", "type_of_violence"),
                                                                    row.numbers = FALSE, feature.id = FALSE), 
                           color = ~pal(type_of_violence),
                           stroke = TRUE, fillOpacity = 0.8)

)


#Table

output$mytable <- DT::renderDataTable({
  DT::datatable(dt_u,extensions = 'Buttons', rownames = FALSE, filter = 'top',
                options = list(lengthMenu = c(5, 30, 50), pageLength = 7,orderClasses = TRUE, 
                               dom = 'Bfltip', buttons=c('copy', 'csv', 'excel', 'print', 'pdf'),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#fef9e7', 'color': '#000'});",
                                 "}")
                               ))
})


# Tree

conflictTree <- reactive(dfdyad[dfdyad$region %in% input$region2, c("year", "region", "country","dyad_name", "Victims")])

output$tree <- renderCollapsibleTree(
  collapsibleTree(
    conflictTree(),
    root = input$region2,
    attribute = "Victims",
    hierarchy = c("country", "dyad_name"),
    fill = "#4C237E",
    zoomable = FALSE
  )
)

output$number_victims <- renderValueBox({
  valueBox(
    value = tags$p(formatC(as.numeric(number_victims), format="f", big.mark=",", digits=0), style = "font-size: 70%;") ,
    subtitle = tags$p("Number of victimes since 1989", style = "font-size: 100%; color: yellow; "),
    icon=tags$i(class = "fas fa-heart-broken", style="font-size: 50px; color: white"),
    width = 1,
    color="purple") 
  
})

output$dyad_most_affected_name <- renderValueBox({
  valueBox(
    value = tags$p(dyad_most_affected[1,1], style = "font-size: 70%;"),
    subtitle = tags$p("Is the dyad most affected by conflicts in terms of victims", style = "font-size: 100%; color: yellow; "),
    icon=tags$i(class = "fas fa-users", style="font-size: 50px; color: white"),
    width = 1,
    color="purple") 
  
})
output$lineoverview <- renderPlot({ 
  
  p <- newggslopegraph(fatality , year , Victims, region,
                       Title = "Conflict fatality evolution",
                       SubTitle = "1989-2020", LineThickness = 2, YTextSize = 4, XTextSize = 20, DataTextSize = 4, DataLabelFillColor = "white", DataTextColor = "black",DataLabelPadding = 0.1) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),plot.background = element_blank(),
        axis.text=element_text(size = 12, face = "bold"))
  ggplot_build(p)
})


}
shinyApp(ui, server)
