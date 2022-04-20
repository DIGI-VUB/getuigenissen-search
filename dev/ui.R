library(shiny)
library(bslib)
library(shinyWidgets)
library(udpipe)
library(stringr)
library(magick)
library(waiter)
library(shinydashboard)
dashdata <- list()
dashdata$DB <- file.path(Sys.getenv("GETUIGENISSEN_HOME"), "src", "search", "data", "DB.rds")
dashdata$DB <- "/srv/shiny-server/search/DB.rds"
dashdata$DB <- readRDS(dashdata$DB)
dashdata$DB$taal <- txt_recode(dashdata$DB$taal, from = c("FR/NL", "NL/FR"), to = c("NL+FR", "NL+FR"))
dashdata$DB$geslacht <- toupper(dashdata$DB$geslacht)
dashdata$DB$geslacht <- txt_recode(dashdata$DB$geslacht, from = c("F"), to = c("V"))
dashdata$DB$locatie.misdrijf <- str_to_title(dashdata$DB$locatie.misdrijf)
dashdata$DB$woonplaats <- str_to_title(dashdata$DB$woonplaats)
dashdata$DB$familienaam <- str_to_title(dashdata$DB$familienaam)
dashdata$DB$beroep <- str_to_title(dashdata$DB$beroep)
dashdata$key_values <- lapply(dashdata$DB[, setdiff(colnames(dashdata$DB), "image_urls")], FUN = function(x) sort(unique(x), decreasing = FALSE))
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "litera"),
                #autoWaiter(),
                useWaiter(),
                tags$style(type="text/css", "#ui_txt {white-space: pre-wrap; overflow-y: auto;}"),
                tags$hr(),
                tags$blockquote("Hier kan je zoeken in de gedigitaliseerde Getuigenissen"),
                #tags$hr(),
                fluidRow(
                  column(1,
                         dropdownButton(
                           tags$h4("Filters"),
                           checkboxGroupButtons(inputId = 'ui_taal', label = 'Taal', choices = c("NL", "FR", "NL+FR"), selected = "NL", justified = TRUE),
                           checkboxGroupButtons(inputId = 'ui_eeuw', label = 'Eeuw', choices = c("18de eeuw", "19de eeuw"), justified = TRUE),
                           tags$h4("Misdrijf"),
                           selectInput(inputId = 'ui_conflict', label = 'Type conflict', choices = dashdata$key_values$conflict, multiple = TRUE),
                           selectInput(inputId = 'ui_locatie_misdrijf', label = 'Locatie misdrijf', choices = dashdata$key_values$locatie.misdrijf, multiple = TRUE),
                           tags$h4("Getuige"),
                           checkboxGroupButtons(inputId = 'ui_getuige_type', label = 'Verdachte/Getuige', choiceValues = c("G", "V"), choiceNames = c("Getuige", "Verdachte"), selected = c("G", "V"), justified = TRUE),
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
                           label = "Zoek in de Getuigenissen",
                           placeholder = "Typ hier woorden in bv (moord, Jean, Assebroecke, café)",
                           btnSearch = icon("search"),
                           #btnReset = icon("remove"),
                           width = "100%"
                         )
                  )
                ),
                fluidPage(
                  fluidRow(
                    column(width = 2,
                           valueBoxOutput(outputId = "uo_query_stats"),
                           tags$hr(),
                           tags$br(),
                           htmlOutput(outputId = "uo_getuigenis")
                    ),
                    column(width = 10,
                           tabsetPanel(
                             tabPanel("Transcriptie", htmlOutput("ui_txt")),
                             tabPanel("Beeld", imageOutput("uo_img"))#,
                             #tabPanel("Table", tableOutput("uo_table"))
                           )
                    )
                  )
                )
)
