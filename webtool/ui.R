# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "orbisant_logo.png", height = '30px', hspace ='30'),
                               ""),
                   position = c("static-top"), windowTitle = "Critical Role: A Statistical Exploration",
                   id = "page_tab",
                   
                   
                   #---------------------------------------------Landing page for overall app----------------------                 
                   tabPanel(navtab0,
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "corp-styles.css")
                            ),
                            
                            setBackgroundImage(src = "mn_light.png"),
                            
                            fluidRow(style = "padding-top: 0", 
                                     HTML("
                                          <center>
                                          <img src= 'cr_logo.png', height = '175px'>
                                          </center>
                                          "  
                                     )
                            ),
                            
                            p(),
                            fluidRow(
                              column(2),
                              column(8, style = "min-height: 150px;",
                                     HTML(splash_page_content)
                              ),
                              column(2)
                            ),
                            fluidRow(
                              column(1),
                              column(1),
                              column(2, style = "border-style: dotted; border-color: #A0E7E5; min-height: 400px;", 
                                     fluidRow(HTML(welcome_box_1), style = "min-height: 300px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_one", "VIEW ANALYSIS", 
                                                                             style = "color: #A0E7E5; background-color: #ffffff; border-color: #A0E7E5"))
                              ),
                              column(1),
                              
                              column(2, style = "border-style: dotted; border-color: #FD62AD; min-height: 400px;",
                                     fluidRow(HTML(welcome_box_2), style = "min-height: 300px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_two", "VIEW MODELLING", 
                                                                             style = "color: #FD62AD; background-color: #ffffff; border-color: #FD62AD"))
                              ),
                              column(1),
                              
                              column(2, style = "border-style: dotted; border-color: #05445E; min-height: 400px;",
                                     fluidRow(HTML(welcome_box_3), style = "min-height: 300px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_three", "VIEW ABOUT SECTION", 
                                                                             style = "color: #05445E; background-color: #ffffff; border-color: #05445E"))
                              ),
                              column(1),
                              column(1)
                              
                             )
                            
                            ),
                   
                   #----------------------Character analysis header---------------------------
                   tabPanel(navtab1,
                            fluidRow(h1("Character Analysis")
                                     ),
                            tabsetPanel(id = "analysis_tabs",
                              tabPanel("High Level Roll Visualisation",
                                sidebarLayout(
                                  sidebarPanel(
                                    h2("Page Details"),
                                    p("This page produces a range of data visualisations for dice roll data by character.")
                                  ),
                                  mainPanel(
                                   fluidRow(column(9,
                                    h3("The Mighty Nein total roll value breakdown by character"),
                                    plotOutput("tile_plot", height = "450px")
                                      )
                                     )
                                    )
                                   )
                                  ),
                              tabPanel("Distribution Analysis",
                                sidebarLayout(
                                   sidebarPanel(
                                     h2("Page Details"),
                                     p("This page produces a range of data visualisations for dice roll data by character.")
                                         ),
                                   mainPanel(
                                    fluidRow(column(9,
                                     h3("Distribution of Total Roll Values"),
                                     plotOutput("ridge_dens", height = "450px")
                                                )
                                              ),
                                    fluidRow(
                                     h3("Episode Time-Series Distribution of Rolls"),
                                     plotOutput("ts_dens", height = "450px")
                                               )
                                             )
                                           )
                                         ),
                              tabPanel("Multinomial Roll Model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2("Page Details"),
                                           p("This page produces a range of data visualisations for dice roll data by character.")
                                         ),
                                   mainPanel(
                                     fluidRow(column(9,
                                      h3("Probability of rolling a Nat1 or Nat20 relative to intercept (made up of other rolls & Beauregard)"),
                                      plotOutput("multinom_plot", height = "450px")
                                             )
                                            )
                                           )
                                          )
                                         )
                                        )
                            ),
                   
                   #----------------------State space modelling header------------------------
                   tabPanel(navtab2,
                            fluidRow(h1("State Space Modelling")
                            ),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  h2("Page Details"),
                                  p("This page produces outputs from a Bayesian state space model that was written to analyse damage dealt and healing given by episode. The analysis may take a few minutes to compute (and the graph to appear) as it is a complex statistical model."),
                                  actionButton("go_ss", "Run Model")
                                ),
                                mainPanel(
                                  fluidRow(column(9,
                                    h3("State Space Model Output"),
                                    shinycssloaders::withSpinner(plotOutput("ss_model", height = "550px")),
                                    br(),
                                    p("Points indicate actual data (aggregated sums per episode across all characters). Lines indicate mean posterior estimates. Shaded areas indicate 95% credible intervals.")
                                   )
                                  )
                                )
                              )
                            )
                          ),
                   
                   #----------------------Help page header------------------------------------
                   tabPanel(navtab3,
                            fluidRow(h1("About")
                            ),
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #05445E; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px;")
                   
)
)
