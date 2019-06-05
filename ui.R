library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(sqldf)
library(shinyBS)


shinyUI(fluidPage(shinyjs::useShinyjs(),dashboardPage(skin="blue",
                                                      dashboardHeader(title = "Auto EDA",titleWidth=260),
                                                      dashboardSidebar(width = 260,
                                                                       sidebarMenu(
                                                                           #menuItem("Recommendation", tabName = "items", icon = icon("star-o")),
                                                                           menuItem("Data Upload", tabName = "data", icon = icon("refresh"),
                                                                                    list(
                                                                                        fileInput('file1', 'Choose file to upload',
                                                                                                  accept = c(
                                                                                                      'text/csv',
                                                                                                      'text/comma-separated-values',
                                                                                                      'text/tab-separated-values',
                                                                                                      'text/plain',
                                                                                                      '.csv',
                                                                                                      '.tsv')),
                                                                                        tags$hr(),
                                                                                        checkboxInput('header','Header',TRUE),
                                                                                        radioButtons('sep','Separator',
                                                                                                     c(Comma=',',Semicolon=';',Tab='\t'),
                                                                                                     ','),
                                                                                        radioButtons('quote', 'Quote',
                                                                                                     c(None='',
                                                                                                       'Double Quote'='"',
                                                                                                       'Single Quote'="'"),
                                                                                                     '"'),
                                                                                        tags$hr())),
                                                                           
                                                                           menuItem("Display", tabName = "disp", icon = icon("star-o")),
                                                                           menuItem("Tell me about the data", tabName = "tab1", icon = icon("table")),
                                                                           
                                                                           menuItem("Relationship between two variables", tabName = "tab2", icon = icon("bar-chart-o")),
                                                                           menuItem("Predictive Analysis", tabName = "tab4", icon = icon("spinner")),
                                                                           menuItem("Error Analysis", tabName = "tab5", icon = icon("bar-chart-o"))
                                                                           
                                                                       )),
                                                      
                                                      
                                                      dashboardBody(
                                                          tags$head(
                                                              tags$style(type="text/css", "select { max-width: 360px; }"),
                                                              tags$style(type="text/css", ".span4 { max-width: 360px; }"),
                                                              tags$style(type="text/css",  ".well { max-width: 360px; }")
                                                          ),
                                                          
                                                          tags$head(tags$style(HTML('
                              .modal.in .modal-dialog{
                              width:100%;
                              height:100%;
                              margin:0px;
                              }
                              
                              .modal-content{
                              width:100%;
                              height:100%;
                              }
                              '))),
                                                          
                                                          
                                                          
                                                          
                                                          
                                                          
                                                          tabItems(tabItem(tabName = "disp",
                                                                           DT::dataTableOutput("table1")
                                                          ),
                                                          
                                                          tabItem(tabName = "tab1",
                                                                  fluidRow(column(6,uiOutput("choose_columns")),
                                                                           column(6,DT::dataTableOutput("data_table"))),
                                                                  HTML('<br/>'),
                                                                  
                                                                  fluidRow(column(6,plotOutput("plot1")),
                                                                           column(6,plotOutput("plot2")))),
                                                          
                                                          tabItem(tabName = "tab2",
                                                                  fluidRow(column(4,uiOutput("choose_columns2")),
                                                                           column(4,uiOutput("choose_columns3")),
                                                                           # column(4,box(width=10,status='info',solidHead = T,
                                                                           #  title = 'Summary Statistics')),
                                                                           tags$head(tags$style(HTML(".small-box{height:150px,width=1000px}"))),
                                                                           column(4,valueBoxOutput(width = 10,'box1')),
                                                                           column(4,valueBoxOutput(width = 10,'box2_temp'))),
                                                                  HTML('<br/>'),plotOutput("plot3")),
                                                          
                                                          tabItem(tabName = "tab3",
                                                                  fluidRow(column(12,DT::dataTableOutput("data_table1_temp"))),
                                                                  DT::dataTableOutput("data_table1")),
                                                          
                                                          tabItem(tabName = "tab4",
                                                                  fluidRow(column(6,uiOutput("choose_columns4")
                                                                  ),
                                                                  column(6,valueBoxOutput(width = 10,'box2'),
                                                                         valueBoxOutput(width = 10,'box3'))),
                                                                  #fluidRow(column(12,plotOutput("plot4"))),
                                                                  HTML('<br/>'),
                                                                  fluidRow(column(6,plotOutput("plot4")),
                                                                           column(6,plotOutput("plot7"))),
                                                                  HTML('<br/>'),
                                                                  fluidRow(column(12,plotOutput("plot5"))),
                                                                  fluidRow(column(12,valueBoxOutput(width = 12,'boxperf')))),
                                                          tabItem(tabName = "tab5",
                                                                  fluidRow(column(4,uiOutput("choose_columns6")),
                                                                           column(4,uiOutput("choose_columns7"))),
                                                                  column(10,plotOutput("ploterror")))
                                                          )))))



