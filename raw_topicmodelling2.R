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

topic_data2    <- expand_grid(ntopics = seq.int(from = 22, to = 30, by = 2),
                             types = list(c("NOUN"),c("ADJ", "NOUN"),c("VERB", "NOUN"),
                                          c("VERB", "ADJ", "NOUN")))

bigram_data2   <- expand_grid(ntopics = seq.int(from = 22, to = 30, by = 2), 
                             types = list(c("NOUN", "Bigrams")))

raw_models2    <- mclapply(X = 1:nrow(topic_data2),
                          FUN = function(x){
                            LDA_modeller(data = ud_raw, 
                                         types = unlist(topic_data2$types[x]), 
                                         ntopics = topic_data2$ntopics[x])
                          }, mc.cores = 3L)

raw_bigram_models2 <- mclapply(X = 1:nrow(bigram_data2),
                          FUN = function(x){
                            LDA_ngram_modeller(data = ud_raw, 
                                               types = unlist(bigram_data2$types[x]), 
                                               ntopics = bigram_data2$ntopics[x])
                          }, mc.cores = 3L)

save(raw_models2, raw_bigram_models2, topic_data2, bigram_data2, file = "LDA_raw_models2.RData")
