library(shiny)
library(bslib)
library(shinyWidgets)
library(udpipe)
library(stringr)
library(magick)
library(waiter)
library(shinydashboard)
dashdata <- list()
dashdata$DB <- "/home/jwijffels/getuigenissen/search/DB.rds"
#dashdata$DB <- file.path(Sys.getenv("GETUIGENISSEN_HOME"), "src", "search", "data", "DB.rds")
#dashdata$DB <- "/srv/shiny-server/search/DB.rds"
dashdata$DB <- readRDS(dashdata$DB)
dashdata$DB$taal <- txt_recode(dashdata$DB$taal, from = c("FR/NL", "NL/FR"), to = c("NL+FR", "NL+FR"))
dashdata$DB$geslacht <- toupper(dashdata$DB$geslacht)
dashdata$DB$geslacht <- txt_recode(dashdata$DB$geslacht, from = c("F"), to = c("V"))
dashdata$DB$locatie.misdrijf <- str_to_title(dashdata$DB$locatie.misdrijf)
dashdata$DB$woonplaats <- str_to_title(dashdata$DB$woonplaats)
dashdata$DB$familienaam <- str_to_title(dashdata$DB$familienaam)
dashdata$DB$beroep <- str_to_title(dashdata$DB$beroep)
dashdata$key_values <- lapply(dashdata$DB[, setdiff(colnames(dashdata$DB), "image_urls")], FUN = function(x) sort(unique(x), decreasing = FALSE))
#“cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, “materia”, “minty”, “pulse”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “yeti”
#cosmo/flatly/litera/umen
# Example of UI with fluidPage
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

#htmlOutput("ui_txt")
#tags$style(type="text/css", "#ui_txt {white-space: pre-wrap; overflow-y: auto;}")



# Server logic
server <- function(input, output) {
  search_results <- reactive({
    query   <- input$ui_search
    queries <- strsplit(query, split = "&")
    queries <- unlist(queries)
    print(queries)
    x   <- dashdata$DB
    if(length(queries) > 0){
      x   <- x[txt_grepl(x = x$text, pattern = queries, ignore.case = TRUE), ]
    }
    x <- subset(x, !is.na(text))
    list(query = queries, data = x)
  })
  search_results_i <- reactive({
    info <- search_results()
    info$row <- 0
    n <- nrow(info$data)
    if(nrow(info$data) > 0){
      info$row  <- sample(seq_len(nrow(info$data)), size = 1)
      info$data <- info$data[info$row, ]
    }
    #print(sprintf("Dataset: %s rows, selected row %s", n, info$row))
    #print(info$data)
    info$text <- info$data$text
    info$url  <- info$data$image_urls
    info$url  <- unlist(info$url)
    info
  })
  output$uo_query_stats <- renderValueBox({
    info <- search_results()
    valueBox(value = sprintf("%s getuigenissen", nrow(info$data)), "")
  })
  output$uo_getuigenis <- renderUI({
    info   <- search_results_i()
    list(subject = tags$h4("Getuigenis rechts"),
         content = tags$ul(
           tags$li("Datum verhoor:", info$data$datum.verhoor),
           tags$li("Locatie misdrijf:", info$data$locatie.misdrijf),
           tags$li("Conflict:", info$data$conflict),
           tags$li("Naam getuige:", info$data$voornaam, info$data$familienaam),
           tags$li("Beroep:", info$data$beroep),
           tags$li("Leeftijd:", info$data$leeftijd),
           tags$li("Woonplaats:", info$data$woonplaats),
           tags$li("Rol:", ifelse(info$data$rol == "G", "Getuige", "Verdachte"))
         ))
  })
  output$ui_txt <- renderUI({
    info       <- search_results_i()
    text       <- info$text
    chunk_text <- info$query
    if(length(chunk_text) > 0){
      lookup     <- chunk_text
      lookup     <- paste(lookup, collapse = "|")
      p     <- gregexpr(pattern = lookup, text = text, ignore.case = TRUE)
      found <- regmatches(text, m = p)
      found <- lapply(found, FUN = function(x) sprintf('<font color=\"#FF0000\"><b>%s</b></font>', x))
      regmatches(text, m = p) <- found
    }
    list(tags$br(),
         subject = tags$blockquote("Zoekterm", ifelse(length(chunk_text) == 0, "-", chunk_text)),
         #content = tags$span(style="white-space: pre", text))
         content = HTML(text))
  })
  output$uo_img <- renderImage({
    info  <- search_results_i()
    waiter_show(id = "uo_img")
    tmpfile <- tempfile(fileext = 'png')
    ok <- try({
      #waiter_show(html = spin_fading_circles())
      image <- image_read(info$url)
      # Numeric operators
      image_write(image, path = tmpfile, format = 'png')
    }, silent = TRUE)

    if(inherits(ok, "try-error")){
      image <- image_blank(width = 400, height = 400)
      image <- image_annotate(
        image, text = "Geen getuigenissen gevonden\nvoor je zoekterm                      \nprobeer een andere combinatie.", size = 30,
        color = "red", boxcolor = "pink",
        degrees = 0, #location = "+50+100",
        font = "Times"
      )
      image_write(image, path = tmpfile, format = 'png')
    }
    waiter_hide(id = "uo_img")
    list(src = tmpfile, contentType = "image/png")
    #tmpfile <- head(info$url, 1)
    #list(src = tmpfile, contentType = "image/png")
    # Return a list

  }, deleteFile = TRUE)
}
shinyApp(ui, server)


