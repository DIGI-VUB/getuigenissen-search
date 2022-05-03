library(magick)
library(data.table)
library(xml2)
library(udpipe)
getuigenissen <- readRDS("data/getuigenissen_2_0.rds")
getuigenissen <- rbindlist(getuigenissen[c("brugse_vrije", "vierschaar_antwerpen")], use.names = TRUE, fill = TRUE, idcol = "project")
getuigenissen$text <- sapply(getuigenissen$value, FUN = function(x) xml_text(read_html(x)))
getuigenissen <- getuigenissen[order(getuigenissen$manifest_id, getuigenissen$canvas_id, decreasing = FALSE), ]

## Save transcriptions to disk
colnames(getuigenissen)
x <- as.data.table(getuigenissen)
x$doc_id           <- x$manifest_id
x$eeuw             <- ifelse(!is.na(x$eeuw), x$eeuw, txt_recode(substr(getuigenissen[["datum verhoor"]], 1, 2), from = c("17", "18"), to = c("18e eeuw", "19e eeuw")))
x$taal             <- getuigenissen$taal
x$conflict         <- getuigenissen$conflict
x$image_id         <- NA_character_
x$datum.zitting    <- NA_character_
x$datum.verhoor    <- getuigenissen[["datum verhoor"]]
x$workflow         <- getuigenissen$archief
x$stad.platteland  <- getuigenissen[["stad-platteland"]]
x$locatie.misdrijf <- ifelse(getuigenissen$archief %in% "RA Brugge", "Brugge", "Antwerpen")
x$familienaam      <- ifelse(!is.na(getuigenissen[["familienaam"]]), getuigenissen[["familienaam"]], getuigenissen[["familienaam getuige"]])
x$voornaam         <- ifelse(!is.na(getuigenissen[["voornaam"]]), getuigenissen[["voornaam"]], getuigenissen[["voornaam getuige"]])
x$rol              <- ifelse(!is.na(getuigenissen$rol), getuigenissen$rol, "G")
x$geslacht         <- toupper(ifelse(!is.na(getuigenissen[["geslacht"]]), getuigenissen[["geslacht"]], getuigenissen[["geslacht getuige"]]))
x$leeftijd         <- NA_character_
x$beroep           <- NA_character_
x$geboorteplaats   <- getuigenissen[["geboorteplaats getuige"]]
x$woonplaats       <- getuigenissen[["woonplaats getuige"]]
x$subject.nr       <- as.integer(gsub("Subject", "", getuigenissen$manifest_label))
DB <- x[, list(text = paste(text, collapse = "\n\n"), 
              image_urls = list(image_url),
              image_urls_n = length(image_url)), 
       by = c("project", "collection_id", "doc_id", "manifest_label", 
              c("taal", "eeuw", "workflow", "datum.zitting", "datum.verhoor", 
                "conflict", "locatie.misdrijf", "stad.platteland", 
                "familienaam", "voornaam", "rol", "geslacht", "leeftijd", 
                "beroep", "geboorteplaats", "woonplaats"))]
DB <- DB[, c("doc_id", "text", "taal", "eeuw", "workflow", "datum.zitting", "datum.verhoor", 
             "conflict", "locatie.misdrijf", "stad.platteland", 
             "familienaam", "voornaam", "rol", "geslacht", "leeftijd", 
             "beroep", "geboorteplaats", "woonplaats", "image_urls", "image_urls_n")]
View(DB)
saveRDS(DB, file = "data/DB.rds", version = 2)


#getuigenissen_2_0 <- readRDS("~/ShinyApps/open/brugse-vrije-search/data/getuigenissen_2_0.rds")
