ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "litera"),
                #autoWaiter(),
                useWaiter(),
                tags$style(type="text/css", "#ui_txt {white-space: pre-wrap; overflow-y: auto;}"),
                #tags$hr(),
                #tags$blockquote("Hier kan je zoeken in de gedigitaliseerde Getuigenissen"),
                #tags$hr(),
                fluidRow(
                  column(1,
                         dropdownButton(
                           tags$h4("Filters"),
                           checkboxGroupButtons(inputId = 'ui_taal', label = 'Taal', choices = c("NL", "FR", "NL+FR"), selected = "NL", justified = TRUE, individual = TRUE, status = "info"),
                           checkboxGroupButtons(inputId = 'ui_eeuw', label = 'Eeuw', choices = c("18de eeuw", "19de eeuw"), justified = TRUE, status = "info"),
                           tags$h4("Misdrijf"),
                           selectInput(inputId = 'ui_conflict', label = 'Type conflict', choices = dashdata$key_values$conflict, multiple = TRUE),
                           selectInput(inputId = 'ui_locatie_misdrijf', label = 'Locatie misdrijf', choices = dashdata$key_values$locatie.misdrijf, multiple = TRUE),
                           tags$h4("Getuige"),
                           checkboxGroupButtons(inputId = 'ui_getuige_type', label = 'Verdachte/Getuige', choiceValues = c("G", "V"), choiceNames = c("Getuige", "Verdachte"), selected = c("G", "V"), justified = TRUE, status = "info"),
                           selectInput(inputId = 'ui_locatie_getuige', label = 'Woonplaats getuige', choices = dashdata$key_values$woonplaats, multiple = TRUE),
                           selectInput(inputId = 'ui_getuige_geslacht', label = 'Geslacht vd getuige', choices = dashdata$key_values$geslacht, multiple = TRUE),
                           selectInput(inputId = 'ui_getuige_familienaam', label = 'Familienaam vd getuige', choices = dashdata$key_values$familienaam, multiple = TRUE),
                           selectInput(inputId = 'ui_getuige_beroep', label = 'Beroep vd getuige', choices = dashdata$key_values$beroep, multiple = TRUE),
                           circle = TRUE, status = "primary", icon = icon("filter"), label = "Metadata",
                           tooltip = tooltipOptions(title = "Klik om te filteren op basis van metadata van de getuigenis")
                         )
                  ),
                  column(11,
                         searchInput(
                           inputId = "ui_search", 
                           label = NULL, 
                           placeholder = "Typ hier je zoekterm", 
                           btnSearch = icon("search"), 
                           btnReset = icon("remove"), 
                           width = "100%"
                         )
                  )
                ),
                fluidPage(
                  conditionalPanel(
                    #condition = "input.ui_search.length > 0",
                    #condition = "true",
                    #condition = "input.current_selection_nr > 0",
                    condition = "output.panel_ready",
                    fluidRow(
                      column(width = 3,
                             valueBoxOutput(outputId = "uo_query_stats"),
                             actionButton(inputId = "ui_ander_sample", label = "Toon een andere getuigenis"),
                             htmlOutput(outputId = "uo_getuigenis")
                      ),
                      column(width = 9, 
                             tabsetPanel(
                               tabPanel("Transcriptie", htmlOutput("ui_txt")),
                               tabPanel("Beeld", 
                                        tags$br(),
                                        htmlOutput(outputId = "uo_beeld_nr"),
                                        fluidRow(
                                          actionButton(inputId = "ui_beeld_previous", label = "", icon = icon("angle-double-left")),
                                          actionButton(inputId = "ui_beeld_next", label = "", icon = icon("angle-double-right"))
                                        ),
                                        tags$br(),
                                        imageOutput("uo_img"))#,
                               #tabPanel("Gelijkaardige zoektermen", 
                               #         htmlOutput("ui_w2v_zoekterm"),
                               #          tableOutput("uo_w2v_table"))
                             )
                      )
                    ) 
                  )
                )
)