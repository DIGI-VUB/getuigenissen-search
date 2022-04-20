library(shiny)
library(bslib)
library(shinyWidgets)
library(udpipe)
library(stringr)
library(magick)
library(waiter)
library(shinydashboard)
dashdata <- list()
dashdata$DB <- "C:/Users/Jan/Vrije Universiteit Brussel/Getuigenissen - Documenten/Data-Verwerking/Getuigenissen/src/search/data/DB.rds"
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
