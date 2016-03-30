shinyUI(navbarPage(title = "Ohio Lepidopterists Long-term Butterfly Monitoring", theme=shinytheme("cosmo"),
                   tabPanel("Monitoring Sites",
                  
                                    fluidRow(
                                      column(4, h2("Species observed"), h3("Explore the species at each site")),
                                      column(8,
                                             fluidRow(
                                               # column(6, selectInput("location", "Monitoring Site", c("", sort(sitesonly$location)), selected="", multiple=F)),
                                               column(6, selectInput("location", "Monitoring Site", sort(sitesonly$location), selected="Penitentiary Glen", multiple=F)),
                                               
                                               column(6, uiOutput("yearOutput"))
                                             )
                                      )
                                    ),
                                    # bsTooltip("location", "Enter a community. The menu will filter as you type. You may also select a community using the map.", "top", options = list(container="body")),
                                    # bsTooltip("dec", "Select decades for projected climate. A 30-year historical baseline is automatically included in the plot.", "top", options = list(container="body")),
                                    # bsTooltip("rcp", "Representative Concentration Pathways, covering a range of possible future climates based on atmospheric greenhouse gas concentrations.", "top", options = list(container="body")),
                                    fluidRow(
                                      column(6, leafletOutput("Map")),
                                      column(6, DT::dataTableOutput("table"))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(6, plotOutput("graph1a")),
                                      column(6, plotOutput("graph1b"))
                                    )
                            # ,
                            #         br(),
                            #         fluidRow(
                            #           column(2, actionButton("help_loc_btn", "About monitoring program", class="btn-block"), br()),
                            #           column(2, actionButton("help_rcp_btn", "About methods", class="btn-block"))
                            #         ),
                            #         bsModal("modal_loc", "Alaska and western Canada communities", "help_loc_btn", size="large",
                            #                 HTML('
                            #                      <p style="text-align:justify">Information can go here.</p>
                            #                      
                            #                      <p style="text-align:justify">Even more can go here.</p>'
                            #                      
                            #                 )),
                            #         
                            #         bsModal("modal_rcp", "Representative Concentration Pathways", "help_rcp_btn", size="large",
                            #                 HTML('
                            #                      <p style="text-align:justify">Explaining where these numbers come from.</p>
                            #                      
                            #                      <p style="text-align:justify">It is real math.</p>'
                            #                 ))
                   ), # close tab 1
                   
                   tabPanel("Species abundance trends",
                            
                            fluidRow(
                              column(4, h2("Species annual abundance"), h3("Compare the site and statewide trends")),
                              column(8,
                                     fluidRow(
                                       column(6, selectizeInput("species", "Common name of species", c("", sort(unique(spec.sites$CommonName))), selected="", multiple=F)),
                                       column(6, uiOutput("siteOutput"))
                                     )
                              )
                            ),
                            # bsTooltip("location", "Enter a community. The menu will filter as you type. You may also select a community using the map.", "top", options = list(container="body")),
                            # bsTooltip("dec", "Select decades for projected climate. A 30-year historical baseline is automatically included in the plot.", "top", options = list(container="body")),
                            # bsTooltip("rcp", "Representative Concentration Pathways, covering a range of possible future climates based on atmospheric greenhouse gas concentrations.", "top", options = list(container="body")),
                            fluidRow(
                              column(6, leafletOutput("Map2")),
                              column(6, plotOutput("graph2a"))
                            )
                            # ,
                            # br(),
                            # fluidRow(
                            #   column(2, actionButton("help_loc_btn", "About monitoring program", class="btn-block"), br()),
                            #   column(2, actionButton("help_rcp_btn", "About methods", class="btn-block"))
                            #   # column(8, h5(HTML(paste(caption, '<a href="http://snap.uaf.edu" target="_blank">snap.uaf.edu</a>'))))
                            # ),
                            # bsModal("modal_loc", "Alaska and western Canada communities", "help_loc_btn", size="large",
                            #         HTML('
                            #              <p style="text-align:justify">Information can go here.</p>
                            #              
                            #              <p style="text-align:justify">Even more can go here.</p>'
                            #              
                            #         )),
                            # 
                            # bsModal("modal_rcp", "Representative Concentration Pathways", "help_rcp_btn", size="large",
                            #         HTML('
                            #              <p style="text-align:justify">Explaining where these numbers come from.</p>
                            #              
                            #              <p style="text-align:justify">It is real math.</p>'
                            #         ))
                                    ), # close tab 2
                   tabPanel("Phenology and weather",
                            
                            fluidRow(
                              column(4, h2("How does weather affect phenology?"), h3("Select sites and species")),
                              column(8,
                                     fluidRow(
                                       # column(6, selectInput("location", "Monitoring Site", c("", sort(sitesonly$location)), selected="", multiple=F)),
                                       column(6, selectInput("location3", "Monitoring Site", sort(sitesonly$location), selected="Penitentiary Glen", multiple=F))
                                       ,
                                       column(6, uiOutput("species3Output"))
                                     )
                              )
                            ),
                            # bsTooltip("location", "Enter a community. The menu will filter as you type. You may also select a community using the map.", "top", options = list(container="body")),
                            # bsTooltip("dec", "Select decades for projected climate. A 30-year historical baseline is automatically included in the plot.", "top", options = list(container="body")),
                            # bsTooltip("rcp", "Representative Concentration Pathways, covering a range of possible future climates based on atmospheric greenhouse gas concentrations.", "top", options = list(container="body")),
                            fluidRow(
                              column(6, leafletOutput("Map3")),
                              column(6, plotOutput("graph3phen"))
                              ),
                            fluidRow(
                              column(6, plotOutput("graph3a")),
                              column(6, plotOutput("graph3b"))
                              )
            
                            # ,
                            #         br(),
                            #         fluidRow(
                            #           column(2, actionButton("help_loc_btn", "About monitoring program", class="btn-block"), br()),
                            #           column(2, actionButton("help_rcp_btn", "About methods", class="btn-block"))
                            #         ),
                            #         bsModal("modal_loc", "Alaska and western Canada communities", "help_loc_btn", size="large",
                            #                 HTML('
                            #                      <p style="text-align:justify">Information can go here.</p>
                            #                      
                            #                      <p style="text-align:justify">Even more can go here.</p>'
                            #                      
                            #                 )),
                            #         
                            #         bsModal("modal_rcp", "Representative Concentration Pathways", "help_rcp_btn", size="large",
                            #                 HTML('
                            #                      <p style="text-align:justify">Explaining where these numbers come from.</p>
                            #                      
                            #                      <p style="text-align:justify">It is real math.</p>'
                            #                 ))
                   ), # close tab 3
                   
                   tabPanel("about",
                            source("about.R",local=T)$value)
                                            ))
