suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(topicmodels))
suppressPackageStartupMessages(require(parallel))
suppressPackageStartupMessages(require(textmineR))

source("R/story_functions.R")
load(file = "data/annotated_text.RData")
set.seed(12345)
ntopics       <- seq.int(from = 4, to = 28, by = 2)


bigram_data   <- expand_grid(ntopics = ntopics,
                             types = list(c("NOUN", "Bigrams")))

raw_bigram_models <- mclapply(X = 1:nrow(bigram_data),
                          FUN = function(x){
                            LDA_ngram_modeller(data = ud_raw,
                                               types = unlist(bigram_data$types[x]),
                                               ntopics = bigram_data$ntopics[x])
                          }, mc.cores = 1L)

save(raw_bigram_models, bigram_data, ntopics, file = "data/LDA_bigram_models.RData")
