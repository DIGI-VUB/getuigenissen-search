library(magick)
library(data.table)
library(readxl)
getuigenissen <- readRDS("data/getuigenissen_20211110.rds")
fields <- c("subject.nr", "eeuw", "datum.zitting", 
  "conflict", "locatie.misdrijf", "stad.platteland", "taal", "datum.verhoor", 
  "familienaam", "voornaam", "rol", "geslacht", "leeftijd", 
  "beroep", "geboorteplaats", "woonplaats", 
  "workflow", "metadata_subject_id", "gold_text", "image_id")
getuigenissen$meta$doc_id <- getuigenissen$meta$subject.nr
getuigenissen$meta$text   <- getuigenissen$meta$gold_text
#View(getuigenissen$meta[, fields])





# x <- getuigenissen$tasks[, c("subject_id", "image_id", "image_urls")]
# x$images_zooniverse <- lapply(x$image_urls, basename)
# x <- setDF(x)
# x <- merge(x, meta[, c("image_id", "images", "eeuw", "workflow", "taal", "txt.files", "gold_text")],
#            by = "image_id")
# x <- setnames(x, old = "images", new = "images_local")
# x$image_id <- NULL


## take url of last version of the tasks
tasks <- setDF(getuigenissen$tasks)
tasks <- tasks[, c("metadata_subject_id", "image_urls")]
tasks <- tasks[!duplicated(tasks$metadata_subject_id), ]
tasks <- unique(tasks)
which(duplicated(tasks$metadata_subject_id))
img <- image_read(tasks$image_urls[[99]])
img

DB <- getuigenissen$meta[, fields]
DB <- merge(DB, tasks, by.x = "subject.nr", by.y = "metadata_subject_id", all.x = TRUE)

tasks <- setDF(getuigenissen$tasks)
tasks <- tasks[order(tasks$updated_at, decreasing = TRUE), ]
tasks <- tasks[, c("image_id", "image_urls")]
tasks <- unique(tasks)
tasks <- tasks[!duplicated(tasks$image_id), ]
DB <- getuigenissen$meta[, fields]
DB <- merge(DB, tasks, by.x = "image_id", by.y = "image_id", all.x = TRUE)

DB$doc_id <- DB$subject.nr
DB$text   <- DB$gold_text
DB$image_urls_n <- sapply(DB$image_urls, FUN = function(x) length(na.exclude(x)))
table(DB$image_urls_n)

DB <- DB[, c("doc_id", "text", "taal", "eeuw", "workflow", "datum.zitting", "datum.verhoor", 
       "conflict", "locatie.misdrijf", "stad.platteland", 
       "familienaam", "voornaam", "rol", "geslacht", "leeftijd", 
       "beroep", "geboorteplaats", "woonplaats", "image_urls", "image_urls_n")]
View(DB)
saveRDS(DB, file = "src/search/data/DB_20211110.rds")
DB <- readRDS(file = "src/search/data/DB_20211110.rds")
