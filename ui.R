library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(Cairo)
library(DT)
library(plotly)

dashboardPage(skin = "black", dashboardHeader(title = "SPC Dashboard" ),
              dashboardSidebar(
                sidebarMenu(
                  
                  menuItem("Karta ImR", tabName = "ImR", icon = icon("line-chart"), badgeLabel = "", badgeColor = "green"),
                  
                  menuItem("Karta XbarR", tabName = "XbarR", icon = icon("area-chart"), badgeLabel = "", badgeColor = "green"),
                  menuItem("Dane", tabName = "Dane", icon = icon("file-o"), badgeLabel = "", badgeColor = "green")
                  
                  
                )
                
              ),
              dashboardBody(
                tags$head(tags$style(HTML('
                                          .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
                                          background-color: #2F4154;
                                          }
                                          .skin-black .sidebar-menu>li.active>a, .skin-black .sidebar-menu>li:hover>a
                                          {
                                          color: rgba(255,255,255,.8);
                                          background: #34495E;
                                          border-left-color: #4c4c4c;
                                          padding: 15px 15px 15px 15px;
                                          display: block;
                                          border-bottom: 1px solid rgba(255,255,255,.05);
                                          }
                                          .skin-black .sidebar-menu>li>a{
                                          color: rgba(255,255,255,.5);
                                          padding: 15px 15px 15px 15px;
                                          display: block;
                                          border-bottom: 1px solid rgba(255,255,255,.05);
                                          }
                                          .box.box-solid.box-primary>.box-header {
                                          color: #FFF;
                                          background: #F6F6F6;
                                          
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 18px;
                                          font-weight: normal;
                                          
                                          }
                                          .box.box-solid.box-primary {
                                          border: 1px solid #e9e9e9;
                                          }
                                          .nav-tabs-custom>.nav-tabs>li.active {
                                          border-top-color: #cccccc;
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 18px;
                                          font-weight: normal;
                                          font-style: italic;
                                          }
                                          .nav-tabs-custom>.nav-tabs>li>a, .nav-tabs-custom>.nav-tabs>li>a:hover {
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 16px;
                                          font-weight: 500;
                                          font-style: normal;
                                          background: 0 0;
                                          margin: 0;
                                          }
                                          .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
                                          display: inline-block;
                                          font-size: 14px;
                                          font-weight: 700;
                                          text-transform: uppercase;
                                          margin: 0;
                                          line-height: 1.42857;
                                          }
                                          .checkbox label {
                                          font-size: 14px;
                                          font-weight: 100;
                                          }
                                          body {
                                          font-family: "Lato", "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 14px;
                                          font-weight: normal;
                                          }
                                          label {
                                          display: inline-block;
                                          max-width: 100%;
                                          margin-bottom: 5px;
                                          font-weight: 500;
                                          }
                                          .main-header .logo {
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 24px;
                                          font-weight: normal;
                                          
                                          }
                                          .h1, .h5, h1, h5 {
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 16px;
                                          line-height: 1.1;
                                          color: inherit;
                                          }
                                          
                                          .h3, h3{
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 14px;
                                          font-weight: 700;
                                          font-style: normal;
                                          line-height: 1.1;
                                          color: #626262;
                                          }
                                          .h2, h2{
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 14px;
                                          font-weight: 700;
                                          font-style: normal;
                                          line-height: 1.1;
                                          color: #626262;
                                          text-transform: uppercase;
                                          }
                                          .h4, h4{
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 18px;
                                          font-weight: 600;
                                          font-style: normal;
                                          line-height: 1.1;
                                          color: #5B90BF;
                                          }
                                          
                                          .h6, h6{
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 14px;
                                          font-weight: normal;
                                          font-style: normal;
                                          line-height: 1.1;
                                          color: inherit;
                                          }
                                          .irs-bar {
                                          height: 8px;
                                          top: 25px;
                                          border-top: 1px solid #2F4154;
                                          border-bottom: 1px solid #2F4154;
                                          background: #2F4154;
                                          }
                                          .irs-bar-edge {
                                          height: 8px;
                                          top: 25px;
                                          width: 14px;
                                          border: 1px solid #2F4154;
                                          border-right: 0;
                                          background: #2F4154;
                                          border-radius: 16px 0 0 16px;
                                          Unknown property name.-moz-border-radius: 16px 0 0 16px;
                                          }
                                          .nav-tabs-custom>.nav-tabs>li.header {
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-weight: 500;
                                          line-height: 35px;
                                          padding: 0 10px;
                                          font-size: 24px;
                                          font-style: normal;
                                          color: #444;
                                          }
                                          .progress-bar {
                                          float: left;
                                          width: 0;
                                          height: 100%;
                                          font-size: 12px;
                                          line-height: 20px;
                                          color: #000000;
                                          text-align: center;
                                          background: #cccccc;
                                          -webkit-box-shadow: inset 0 -1px 0 rgba(0,0,0,.15);
                                          box-shadow: inset 0 -1px 0 rgba(0,0,0,.15);
                                          -webkit-transition: width .6s ease;
                                          Unknown property name.-o-transition: width .6s ease;
                                          transition: width 3s ease;
                                          }
                                          .pagination>.active>a, .pagination>.active>a:focus, .pagination>.active>a:hover, .pagination>.active>span, .pagination>.active>span:focus, .pagination>.active>span:hover {
                                          z-index: 2;
                                          color: #fff;
                                          cursor: default;
                                          background-color: #cccccc;
                                          border-color: #cccccc;
                                          }
                                          .sidebar {
                                          font-size: 13px;
                                          color: #444;
                                          font-weight: 700;
                                          }
                                          .selectize-input, .selectize-input input {
                                          color: #333333;
                                          font-family: inherit;
                                          font-size: inherit;
                                          line-height: 23px;
                                          
                                          }
                                          .box.box-solid.box-primary>.box-header .btn, .box.box-solid.box-primary>.box-header a {
                                          color: #34495E;}
                                          
                                          '))),
                
                tabItems(
                  tabItem(tabName = "Dane",
                          fluidRow(column(width=7,
                                          box(title = "Otwórz plik", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                              fluidRow(
                                                column(width=6, fileInput('file1', label = h4("ImR"), accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                column(width=6, fileInput('file2', label = h4("XbarR"), accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))
                                                
                                              )
                                              
                                              
                                              
                                          )# koniec BOX
                                          
                          ))),
                  tabItem(tabName = "ImR",
                          fluidRow(
                            tabBox(side="left",width = 12, title = "SPC - statystyczna obserwacja procesu", selected = "Karta ImR", 
                                   
                                   tabPanel(title = "Karta ImR", br(),
                                            fluidRow(
                                              column(width=12,
                                                     plotOutput('kartaI', height = 300, click =clickOpts("plot_click", clip=FALSE)),
                                                     
                                                     br(), br(),br(),
                                                     plotOutput('karta.mR', height = 150)
                                                     
                                              ) # koniec column
                                            ), br(),br(),br(),# koniec fluidRow
                                            fluidRow(
                                              column(width=3
                                                     #sliderInput("slider2", label = NULL, min = 0, 
                                                     #            max = 100, value = c(40, 60))
                                              )
                                            )
                                   ),#koniec tabpanel Wykresy
                                   tabPanel(title = "Ustawienia",
                                            fluidRow(
                                              column(width=7,
                                                     box(title = "Ustawienia karty ImR", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         fluidRow(
                                                           column(width=6, 
                                                                  numericInput("bound.ImR.LCL", label = h3("Limit dolny"), value = 0),
                                                                  checkboxInput("checkbox.ImR.LCL", label = "Ustaw", value = FALSE)),
                                                           column(width=6,
                                                                  numericInput("bound.ImR.UCL", label = h3("Limit górny"), value = 0),
                                                                  checkboxInput("checkbox.ImR.UCL", label = "Ustaw", value = FALSE))
                                                         ),#koniec fluidRow
                                                         fluidRow(
                                                           column(width=12,
                                                                  hr(),sliderInput("slider.ImR", label = h3("Kalkulacja limitów"),min = 5,max = 50,value = 20),
                                                                  hr(),h3("Resetuj podział na podgrupy"), actionButton("resetButton", "Restuj")))#koniec fluidRow
                                                     ),
                                                     box(title = "Help", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         fluidRow(
                                                           column(width=12,h2("Limit górny / limit dolny"),
                                                                  h6("Ustaw stałą wartość górnej i/lub dolnej granicy kontrolnej w przypadku gdy wyliczone limity przekraczają realne wartości przyjmowane przez obserwowany wskaźnik"),
                                                                  h2("Kalkulacja limitów"), h6("Ustal, które punkty obserwowanego wskaźnika mają posłużyć do kalkulacji limitów kontrolnych")
                                                           )
                                                         )
                                                     ))
                                              #koniec Column
                                            ) # koniec fluidRow
                                   ),
                                   
                                   tabPanel(title = "Tabela", br(),
                                            fluidRow(  
                                              column(width=6,    DT::dataTableOutput('plot.tabela.ImR')
                                              )) # koniec column
                                   ) #koniec tabPanel Dane
                            )) #koniec tabBox
                  ), #koniec tab "ImR"
                  tabItem(tabName= "XbarR",
                          fluidRow(
                            tabBox(side="left",width = 12, title = "SPC - statystyczna kontrola procesu", selected = "Karta XbarR", 
                                   tabPanel(title = "Karta XbarR", br(),
                                            fluidRow(
                                              column(width=12,
                                                     plotOutput('graficznaX', height = 170), br(),
                                                     plotOutput('graficznaR', height = 170)
                                                     #grVizOutput('sampling_tree', height = 190)
                                                     
                                              ) # koniec column
                                            ), br(),br(),br(),# koniec fluidRow
                                            fluidRow(
                                              column(width=3
                                                     #sliderInput("slider2", label = NULL, min = 0, 
                                                     #            max = 100, value = c(40, 60))
                                              )
                                            )
                                   ),#koniec tabpanel Wykresy
                                   tabPanel(title = "Ustawienia",
                                            fluidRow(column(width=7,
                                                            box(title = "Ustawienia karty XbarR", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                                fluidRow(
                                                                  column(width=6, numericInput("bound.XbarR.LCL", label = h3("Limit dolny"), value = 0),checkboxInput("checkbox.XbarR.LCL", label = "Ustaw", value = FALSE)),
                                                                  column(width=6, numericInput("bound.XbarR.UCL", label = h3("Limit górny"), value = 0),checkboxInput("checkbox.XbarR.UCL", label = "Ustaw", value = FALSE))),#koniec fluidRow
                                                                fluidRow(
                                                                  column(width=12, sliderInput("slider.XbarR", label = h3("Kalkulacja limitow"),min = 5,max = 50,value = 20)))#koniec fluidRow
                                                            ),
                                                            box(title = "Help", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                                fluidRow(
                                                                  column(width=12,h2("Limit górny / limit dolny"),h6("Ustaw stałą wartość górnej i/lub dolnej granicy kontrolnej w przypadku gdy wyliczone limity przekraczają realne wartości przyjmowane przez obserwowany wskaźnik"),
                                                                         h2("Kalkulacja limitów"), h6("Ustal, które punkty obserwowanego wskaźnika mają posłużyć do kalkulacji limitów kontrolnych")))))
                                                     #koniec Column
                                            ) # koniec fluidRow
                                   ),
                                   tabPanel(title = "Tabela", br(),
                                            fluidRow(  
                                              column(width=12,   DT::dataTableOutput('plot.tabela.XbarR')
                                              )) # koniec column
                                   ) #koniec tabPanel Dane
                            )) #koniec tabBox
                  )
                ) # koniec TAB ITEMS
                ) # koniec DASHBOARD BODY
                )
