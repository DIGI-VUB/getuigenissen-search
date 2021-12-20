# Server logic
server <- function(input, output) {
  current_image_nr <- reactiveVal(value = 1)
  search_results <- reactive({
    query   <- input$ui_search
    filters <- list()
    filters$taal             <- input$ui_taal
    filters$eeuw             <- input$ui_eeuw
    filters$conflict         <- input$ui_conflict
    filters$locatie.misdrijf <- input$ui_locatie_misdrijf
    filters$rol              <- input$ui_getuige_type
    filters$woonplaats       <- input$ui_locatie_getuige
    filters$geslacht         <- input$ui_getuige_geslacht
    filters$familienaam      <- input$ui_getuige_familienaam
    filters$beroep           <- input$ui_getuige_beroep

    queries <- strsplit(query, split = "&")
    queries <- unlist(queries)
    queries <- trimws(queries)
    x       <- dashdata$DB
    for(field in names(filters)){
      if(length(filters[[field]]) > 0){
        x <- x[x[[field]] %in% filters[[field]],]
      }
    }
    if(length(queries) > 0){
      x   <- x[txt_grepl(x = x$text, pattern = queries, ignore.case = TRUE), ]
    }else{
      x <- x[0, ]
    }
    x <- subset(x, !is.na(text))
    list(query = queries, data = x)
  })
  search_results_i <- reactive({
    input$ui_ander_sample
    info <- search_results()
    isolate(current_image_nr(1))
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
    info$url_n <- length(info$url)
    info
  })
  observeEvent(input$ui_beeld_next, {
    info <- search_results_i()
    if(current_image_nr() < info$url_n){
      current_image_nr(current_image_nr() + 1)
    }
  })
  observeEvent(input$ui_beeld_previous, {
    info <- search_results_i()
    if(current_image_nr() > 1 & info$url_n){
      current_image_nr(current_image_nr() - 1)
    }
  })
  output$uo_beeld_nr <- renderUI({
    info <- search_results_i()
    current_img_i <- current_image_nr()
    tags$p(sprintf("Toont beeld %s/%s", current_img_i, info$url_n))
  })
  output$uo_query_stats <- renderValueBox({
    info <- search_results()
    valueBox(value = sprintf("%s teksten", nrow(info$data)), "")
  })
  na_to_value <- function(x, value){
    x[is.na(x)] <- value
    x
  }
  output$uo_getuigenis <- renderUI({
    info   <- search_results_i()
    if(nrow(info$data) > 0){
      list(subject = tags$h5("Getuigenis"), 
           content = tags$ul(
             tags$li("Datum verhoor:", na_to_value(info$data$datum.verhoor, "Onbekend")),
             tags$li("Locatie misdrijf:", na_to_value(info$data$locatie.misdrijf, "Onbekend")),
             tags$li("Conflict:", na_to_value(info$data$conflict, "Onbekend")),
             tags$li("Naam getuige:", info$data$voornaam, info$data$familienaam),
             tags$li("Beroep:", na_to_value(info$data$beroep, "Onbekend")),
             tags$li("Leeftijd:", na_to_value(info$data$leeftijd, "Onbekend")),
             tags$li("Woonplaats:", na_to_value(info$data$woonplaats, "Onbekend")),
             tags$li("Rol:", ifelse(info$data$rol == "G", "Getuige", "Verdachte"))
           ))
    }else{
      list()
    }
  })
  output$ui_txt <- renderUI({
    info       <- search_results_i()
    text       <- info$text
    chunk_text <- info$query
    if(length(chunk_text) > 0){
      lookup <- chunk_text
      lookup <- strsplit(lookup, split = "&")
      lookup <- unlist(lookup)
      lookup <- paste(lookup, collapse = "|")
      p      <- gregexpr(pattern = lookup, text = text, ignore.case = TRUE)
      found  <- regmatches(text, m = p)
      found  <- lapply(found, FUN = function(x) sprintf('<font color=\"#FF0000\"><b>%s</b></font>', x))
      regmatches(text, m = p) <- found
    }
    if(length(chunk_text) == 0){
      hoofd <- tags$br()
    }else{
      hoofd <- tags$blockquote(sprintf("Je zoekterm was: %s", chunk_text))
    }
    
    list(hoofd = hoofd, 
         #tags$h6("Getuigenis transcriptie"),
         #content = tags$span(style="white-space: pre", text))
         content = HTML(text))
  })
  output$uo_img <- renderImage({
    beeld_i <- current_image_nr()
    info  <- search_results_i()
    waiter_show(id = "uo_img")
    tmpfile <- tempfile(fileext = 'png')
    showNotification("Het beeld wordt ingeladen, een beetje geduld.", id = "noot", type = "default")
    ok <- try({
      image <- image_read(info$url[beeld_i])
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
    removeNotification(id = "noot")
    list(src = tmpfile, contentType = "image/png") 
  }, deleteFile = TRUE)
  
  output$uo_w2v_table <- renderTable({
    info       <- search_results_i()
    chunk_text <- info$query
    if(length(chunk_text) > 0){
      lookup <- chunk_text
      lookup <- strsplit(lookup, split = "&")
      lookup <- unlist(lookup)
      lookup <- strsplit(lookup, split = "\\|")
      lookup <- unlist(lookup)
      lookup <- txt_clean_word2vec(lookup, ascii = TRUE, tolower = TRUE, trim = TRUE, alpha = TRUE)
      lookup <- intersect(lookup, dashdata$w2v_terms)
      if(length(lookup) > 0){
        out           <- predict(dashdata$w2v, newdata = lookup, top_n = 10, type = "nearest")
        out           <- do.call(rbind, out)
        rownames(out) <- NULL
        out
      }else{
        data.frame(term1 = chunk_text, term2 = "dit woord zat niet in de database")
      }
    }
  })
  output$ui_w2v_zoekterm <- renderUI({
    info       <- search_results_i()
    chunk_text <- info$query
    if(length(chunk_text) == 0){
      hoofd <- tags$br()
    }else{
      hoofd <- tags$blockquote(sprintf("Hieronder vind je woorden die (adhv een word2vec model) lijken op je zoekterm: %s", chunk_text))
    }
    list(hoofd = hoofd)
  })  
}