# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "orbisant_logo.png", height = '50px', hspace = '30'),
                               ""),
                   position = c("static-top"), windowTitle = "Critical Role Statistical Visualiser",
                   id = "page_tab",
                   
                   
                   #---------------------------------------------Landing page for overall app----------------------                 
                   tabPanel(navtab0,
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "corp-styles.css")
                            ),
                            
                            setBackgroundImage(src = "mn_lightest.png"),
                            
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
                              )
                             ),
                            fluidRow(h3("")),
                            fluidRow(h3("")),
                            fluidRow(h3(""))
                            ),
                   
                   #----------------------Spellcasting page header----------------------------
                   tabPanel(navtab5,
                            fluidRow(h1("Spellcasting Analysis")
                            ),
                            tabsetPanel(id = "spellcasting_tabs",
                              tabPanel("Overall Spellcasting",
                              fluidRow(column(11,
                                              h3("Breakdown by Level and Spell Name"),
                                              radioButtons("spell_selector", "", choices = the_spell_levels,
                                                           selected = the_spell_levels[1], inline = TRUE),
                                              shinycssloaders::withSpinner(highchartOutput("spell_tree", height = "550px"))
                               )
                              ),
                              fluidRow(column(11,
                                              h3("Total Spellcasting Counts by Spell Level"),
                                              shinycssloaders::withSpinner(highchartOutput("spell_bar", height = "550px"))
                               )
                              )
                             ),
                           tabPanel("Spellcasting by Character",
                                    fluidRow(column(11,
                                                    h3("Spells by Character and Level"),
                                    radioButtons("network_character", "Which character do you want to visualise?", choices = sankey_list,
                                                 selected = sankey_list[1], inline = TRUE),
                                    tabsetPanel(id = "spell_ch_tabs",
                                      tabPanel("Network Visualisation",
                                    p("Use your mouse or trackpad to zoom in and drag the plot around to better see the spells used."),
                                    br(),
                                    shinycssloaders::withSpinner(simpleNetworkOutput("sankey_plot", height = "550px"))
                                      ),
                                    tabPanel("Sankey Diagram Visualisation",
                                      p("Hover over a pathway with your mouse to see the total number of casts."),
                                      br(),
                                      shinycssloaders::withSpinner(sankeyNetworkOutput("ch_spell_sankey", height = "825px"))
                                )
                               )
                              )
                             )
                            )
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
                                         ),
                              tabPanel("Episodic Damage & Healing",
                                       fluidRow(column(11,
                                                       h3("Damage by Episode"),
                                                       p("Hover over a point to see more information about it."),
                                                       br(),
                                                       shinycssloaders::withSpinner(plotlyOutput("loess_dam", height = "550px"))
                                           )
                                          ),
                                       fluidRow(column(11,
                                                       h3("Healing by Episode"),
                                                       p("Hover over a point to see more information about it."),
                                                       br(),
                                                       shinycssloaders::withSpinner(plotlyOutput("loess_heal", height = "550px"))
                                           )
                                          )
                                         )
                                        )
                                       ),
                   
                   #----------------------Potions page header---------------------------------
                   tabPanel(navtab6,
                            fluidRow(h1("Potions Analysis")
                            ),
                            fluidRow(column(11,
                                            h3("Potion Administration by Character"),
                                            p("Hover over a path to see the total healing provided by potions."),
                                            br(),
                                            shinycssloaders::withSpinner(sankeyNetworkOutput("potion_sankey", height = "550px"))
                            )
                           )
                          ),
                   
                   #----------------------Money page header-----------------------------------
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
                                                h3("Finances by Episode"),
                                                shinycssloaders::withSpinner(plotOutput("waterfall", height = "550px")),
                                                br(),
                                                p("All values are adjusted to their gold equivalent.")
                                )
                               )
                              )
                             )
                            )
                           ),
                   
                   #----------------------Vox Machina page header-----------------------------
                   tabPanel(navtab7,
                            fluidRow(h1("Vox Machina")
                            ),
                            tabsetPanel(id = "vm_tabs",
                                        tabPanel("Spellcasting",
                                                 fluidRow(column(11,
                                                                 h3("Total Spellcasting Counts by Spell Level"),
                                                                 shinycssloaders::withSpinner(highchartOutput("spell_bar_vm", height = "550px"))
                                                  )
                                                 ),
                                                 fluidRow(column(11,
                                                                 h3("Breakdown by Level and Spell Name"),
                                                                 radioButtons("spell_selector_vm", "", choices = the_spell_levels_vm,
                                                                              selected = the_spell_levels_vm[1], inline = TRUE),
                                                                 shinycssloaders::withSpinner(highchartOutput("spell_tree_vm", height = "550px"))
                                                  )
                                                 )
                                                ),
                                        tabPanel("Combat Damage",
                                                 fluidRow(column(11,
                                                                 h3("Overview of Character Combat"),
                                                                 p("Hover over a point to see details."),
                                                                 column(5,
                                                                        h4("Total Damage Dealt and Taken and Total Times KO'd"),
                                                                        radioButtons("name_selec_1", "Do you want character names on the plot?", choices = cluster_choices,
                                                                                     selected = cluster_choices[1], inline = TRUE),
                                                                        shinycssloaders::withSpinner(plotlyOutput("bubble_vm", height = "550px"))),
                                                                 column(1),
                                                                 column(5,
                                                                        h4("Average Damage Dealt and Taken and Total Times KO'd"),
                                                                        radioButtons("name_selec_2", "Do you want character names on the plot?", choices = cluster_choices,
                                                                                     selected = cluster_choices[1], inline = TRUE),
                                                                        shinycssloaders::withSpinner(plotlyOutput("bubble_vm_av", height = "550px")))
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
