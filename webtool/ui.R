# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "orbisant_logo.png", height = '50px', hspace = '30'),
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
                              column(2, style = "border-style: dotted; border-color: #FEB06A; min-height: 300px; max-height: 500px", 
                                     fluidRow(HTML(welcome_box_1), style = "min-height: 250px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_one", "VIEW ANALYSIS", 
                                                                             style = "color: #FEB06A; background-color: #ffffff; border-color: #FEB06A"))
                              ),
                              column(1),
                              
                              column(2, style = "border-style: dotted; border-color: #FD62AD; min-height: 300px; max-height: 500px",
                                     fluidRow(HTML(welcome_box_2), style = "min-height: 250px; margin-left: 20px; margin-right: 20px;"),
                                     fluidRow(align = "center", actionButton("button_two", "VIEW MODELLING", 
                                                                             style = "color: #FD62AD; background-color: #ffffff; border-color: #FD62AD"))
                              ),
                              column(1),
                              
                              column(2, style = "border-style: dotted; border-color: #05445E; min-height: 300px; max-height: 500px",
                                     fluidRow(HTML(welcome_box_3), style = "min-height: 250px; margin-left: 20px; margin-right: 20px;"),
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
                                    p("This page produces a range of data visualisations for dice roll data by character."),
                                    selectInput("bar_input", "Select a roll value", choices = the_roll_values,
                                                selected = the_roll_values[1], multiple = FALSE)
                                  ),
                                  mainPanel(
                                    fluidRow(column(9,
                                     h3("The Mighty Nein counts of rolls by character"),
                                     shinycssloaders::withSpinner(plotOutput("bar_plot", height = "450px"))
                                      )
                                    ),
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
                                     p("This page produces a range of data visualisations for dice roll data by character."),
                                     selectInput("dist_char_selector", "Select a character", choices = the_nein,
                                                 selected = the_nein[1], multiple = FALSE),
                                     radioButtons("dist_ep_selector", "What type of episodes are you interested in?", choices = lev_choices,
                                                 selected = lev_choices[1], inline = TRUE),
                                     radioButtons("gam_zero_selector", "For bottom graph: Do you want episodes where zeros were recorded to be included?", choices = cluster_choices,
                                                  selected = cluster_choices[1], inline = TRUE)
                                         ),
                                   mainPanel(
                                    fluidRow(column(9,
                                     h3("Distribution of total roll values (excl. Nat1 and Nat20)"),
                                     shinycssloaders::withSpinner(plotlyOutput("ridge_dens", height = "450px"))
                                                )
                                              ),
                                    fluidRow(column(9,
                                     h3("Damage dealt and healing given"),
                                     shinycssloaders::withSpinner(plotOutput("lm_plot", height = "450px")),
                                     br(),
                                     p("Shaded region uses a Generalised Additive Model if zeros are allowed and a Linear Model if not. Ribbon indicates 80% confidence interval for the model smoothing estimates.")
                                               )
                                              )
                                             )
                                            )
                                           ),
                              tabPanel("Cluster Analysis",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2("Page Details"),
                                           p("This page produces a range of data visualisations for some character-based statistical analysis."),
                                           sliderInput("num_clus", "How many clusters do you want the analysis to output?",
                                                       min = 1, max = 6, value = 3),
                                           radioButtons("cluster_zero_selector", "Do you want episodes where zeros were recorded to be included?", choices = cluster_choices,
                                                        selected = cluster_choices[1], inline = TRUE)
                                         ),
                                   mainPanel(
                                     fluidRow(column(9,
                                      h3("Cluster analysis"),
                                      plotlyOutput("cluster_plot", height = "450px"),
                                      br(),
                                      p("k-means cluster algorithm was used.")
                                             )
                                            )
                                           )
                                          )
                                         )
                                        )
                                       ),
                   tabPanel(navtab4,
                     fluidRow(h1("Money Analysis")
                            ),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  h2("Page Details"),
                                  p("This page analyses The Mighty Nein's income and expenses by episode."),
                                  selectInput("money_episodes", "Select an episode", choices = the_episodes,
                                              selected = the_episodes[1], multiple = FALSE)
                              ),
                              mainPanel(
                                fluidRow(column(9,
                                                h3("Net Gold by Episode"),
                                                shinycssloaders::withSpinner(plotOutput("waterfall", height = "550px")),
                                                br(),
                                                p("All values are adjusted to their gold equivalent.")
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
                                  p("This page produces outputs from a Bayesian state space model that was written to analyse damage dealt and healing given by episode.")
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
