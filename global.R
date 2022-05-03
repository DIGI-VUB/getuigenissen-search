library(shiny)
library(bslib)
library(shinyWidgets)
library(udpipe)
library(stringr)
library(magick)
library(waiter)
library(shinydashboard)
library(word2vec)
library(quanteda)
library(DT)
#library(reactable)
dashdata <- list()
dashdata$DB <- file.path(Sys.getenv("GETUIGENISSEN_SEARCH_HOME", unset = "/srv/shiny-server/getuigenissen/search"), "data", "DB.rds")
dashdata$DB <- readRDS(dashdata$DB)
dashdata$DB$text             <- iconv(dashdata$DB$text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?")
dashdata$DB$taal             <- txt_recode(dashdata$DB$taal, from = c("FR/NL", "NL/FR"), to = c("NL+FR", "NL+FR"))
dashdata$DB$geslacht         <- toupper(dashdata$DB$geslacht)
dashdata$DB$geslacht         <- txt_recode(dashdata$DB$geslacht, from = c("F"), to = c("V"))
dashdata$DB$locatie.misdrijf <- str_to_title(dashdata$DB$locatie.misdrijf)
dashdata$DB$woonplaats       <- str_to_title(dashdata$DB$woonplaats)
dashdata$DB$familienaam      <- str_to_title(dashdata$DB$familienaam)
dashdata$DB$beroep           <- str_to_title(dashdata$DB$beroep)
dashdata$key_values          <- lapply(dashdata$DB[, setdiff(colnames(dashdata$DB), "image_urls")], FUN = function(x) sort(unique(x), decreasing = FALSE))
dashdata$w2v       <- file.path(Sys.getenv("GETUIGENISSEN_SEARCH_HOME", unset = "/srv/shiny-server/getuigenissen/search"), "data", "w2v.bin")
dashdata$w2v       <- read.word2vec(dashdata$w2v)
dashdata$w2v_terms <- summary(dashdata$w2v)
dlg <- modalDialog(title = "Uw zoekterm in zijn context", 
                   DT::dataTableOutput("uo_kwic"), 
                   #reactable::reactableOutput("uo_kwic"),
                   size = "xl",
                   easyClose = TRUE, footer = modalButton("Sluit"))


