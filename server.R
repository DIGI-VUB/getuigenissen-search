# Server logic
server <- function(input, output, session) {
  showModal(modalDialog("Om te zoeken in de getuigenissen ",
                        tags$ul(
                          tags$li("typ een woord en klik op het zoekicoontje"),
                          tags$li("links kan je optioneel filteren aan de hand van selectiecriteria (bv verdachte of getuige / 18e-19e eeuw / type misdrijf / regio / taal / ...)"),
                        ), size = "l",
                        easyClose = TRUE, footer = modalButton("OK")))  
  current_image_nr     <- reactiveVal(value = 1)
  current_selection_nr <- reactiveVal(value = 0)
  output$panel_ready <- reactive({
    print(current_selection_nr() > 1)
    current_selection_nr() > 1
  })
  observe({
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
    isolate(current_selection_nr(current_selection_nr() + 1))
  })
  outputOptions(output, "panel_ready", suspendWhenHidden = FALSE)
  search_results <- reactive({
    isolate(i <- current_selection_nr())
    i <- current_selection_nr()
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
      if(sum(nchar(queries)) > 0){
        #x <- head(x, n = 0) 
      }
    }
    fields       <- c("ui_search", "ui_taal", "ui_eeuw", "ui_conflict", "ui_locatie_misdrijf", "ui_getuige_type", "ui_locatie_getuige", "ui_getuige_geslacht", "ui_getuige_familienaam", "ui_getuige_beroep")
    fields_label <- gsub(pattern = "ui_", replacement = "", x = fields)
    msg <- mapply(fields, fields_label, FUN = function(field, label){
      content <- input[[field]]
      if(field == "ui_getuige_type"){
        content <- txt_recode(content, from = c("G", "V"), to = c("Getuige", "Verdachte"))
      }
      if(field == "ui_search" && sum(nchar(queries)) == 0){
        return(NULL)
      }
      if(length(content) == 0){
        return(NULL) 
      }else{
        return(tags$li(paste(label, ": ", paste(content, collapse = " + "), collapse = ""))  )
      }
      
    }, SIMPLIFY = FALSE)
    kw <- data.frame(docname = character(), from = integer(), to = integer(), pre = character(), keyword = character(), post = character(), pattern = factor(character()), stringsAsFactors = FALSE)
    if(length(queries) > 0){
      try({
        corp <- corpus(x, docid_field = "doc_id", text_field = "text")
        kw   <- quanteda::kwic(corp, pattern = queries, valuetype = "regex", window = 8, case_insensitive = TRUE)  
        output$uo_kwic <- renderDataTable({
        #output$uo_kwic <- renderReactable({
          prt <- data.frame(
            voor = format(stringi::stri_replace_all_regex(kw$pre, "(\\w*) (\\W)", "$1$2"), justify = "right"), 
                             #s1 = rep("|", nrow(kw)), 
                             zoekterm = format(kw$keyword, justify = "centre"), 
                             #s2 = rep("|", nrow(kw)), 
                             na = format(stringi::stri_replace_all_regex(kw$post, "(\\w*) (\\W)", "$1$2"), justify = "left"), 
            getuige = txt_recode(kw$docname, 
                                 from = attr(kw, "docvars")$docname, 
                                 to = sprintf("%s %s", txt_recode(attr(kw, "docvars")$voornaam, from = NA, to = ""), txt_recode(attr(kw, "docvars")$familienaam, from = NA, to = ""))),
                             stringsAsFactors = FALSE)
          #print(prt, row.names = FALSE)
          colnames(prt)[c(1,3)] <- c("", "")
          DT::datatable(prt,
                        rownames = FALSE,
                        filter = "none",
                        options = list(dom = 'tp',
                                       columnDefs = list(list(className = 'dt-right', targets = 0),
                                                         list(className = 'dt-center', targets = 1),
                                                         list(className = 'dt-left', targets = 2))
                        ))
          # reactable::reactable(
          #   prt,
          #   filterable = FALSE, pagination = FALSE,# highlight = TRUE, height = 250,
          #   columns = list(
          #     voor = colDef(filterable = FALSE, name = "", align = "right"),
          #     zoekterm = colDef(filterable = FALSE, align = "center"),
          #     na = colDef(filterable = FALSE, name = "", align = "left")
          #   )
          # )
        })
      })
      
    }
    if(nrow(x) > 0){
      isolate({
        if(current_selection_nr() > 1){
          showModal(modalDialog(sprintf("Er zijn %s getuigenissen voor je selectiecriteria", nrow(x)), 
                                tags$ul(msg),
                                size = "xl", footer = modalButton("OK"), easyClose = FALSE))
        }  
      })
      if(nrow(kw) > 0){
        showModal(dlg)    
      } 
    }else{
      if(i > 1){
        showNotification(type = "error", "Geen teksten gevonden. Zet je selectie criteria minder scherp om meer teksten te bekomen.") 
      }
      
    }
    x <- subset(x, !is.na(text))
    #isolate(current_selection_nr(current_selection_nr() + 1))
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
             tags$li("Plaats misdrijf:", na_to_value(info$data$locatie.misdrijf, "Onbekend")),
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
    showNotification("Het beeld wordt ingeladen, een beetje geduld.", id = "noot", type = "default", duration = 10)
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