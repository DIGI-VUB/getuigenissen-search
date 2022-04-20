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
