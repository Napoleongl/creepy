suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(topicmodels))
suppressPackageStartupMessages(require(parallel))
suppressPackageStartupMessages(require(textmineR))

source("story_functions.R")
load(file = "annotated_text.RData")
set.seed(831117)
ntopics       <- seq.int(from = 4, to = 32, by = 2)

topic_data    <- expand_grid(ntopics = ntopics,
                             types = list(c("NOUN"),c("ADJ", "NOUN"),c("VERB", "NOUN"),
                                          c("VERB", "ADJ", "NOUN")))

bigram_data   <- expand_grid(ntopics = ntopics, 
                             types = list(c("NOUN", "Bigrams")))

raw_models    <- mclapply(X = 1:nrow(topic_data),
                          FUN = function(x){
                            LDA_modeller(data = ud_raw, 
                                         types = unlist(topic_data$types[x]), 
                                         ntopics = topic_data$ntopics[x])
                          }, mc.cores = 3L)

raw_bigram_models <- mclapply(X = 1:nrow(bigram_data),
                          FUN = function(x){
                            LDA_ngram_modeller(data = ud_raw, 
                                               types = unlist(bigram_data$types[x]), 
                                               ntopics = bigram_data$ntopics[x])
                          }, mc.cores = 3L)

save(raw_models, raw_bigram_models, topic_data, bigram_data, ntopics, file = "LDA_raw_models2.RData")
