library(word2vec)
x      <- readRDS("data/DB_20211110.rds")
x$text <- txt_clean_word2vec(x$text, ascii = TRUE, tolower = TRUE, trim = TRUE, alpha = TRUE)

w2v <- word2vec(x$text, type = "skip-gram", iter = 20, min_count = 2, dim = 50)
write.word2vec(w2v, "data/w2v.bin")

w2v <- read.word2vec("data/w2v.bin")
predict(w2v, newdata = c("huysvrauwe", "moord", "slag"))

