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
set.seed(1111)
ntopics       <- seq.int(from = 4, to = 28, by = 2)

topic_data    <- expand_grid(ntopics = ntopics,
                             types = list(c("NOUN"),c("ADJ", "NOUN"),c("VERB", "NOUN"),
                                          c("VERB", "ADJ", "NOUN")))

raw_models    <- mclapply(X = 1:nrow(topic_data),
                          FUN = function(x){
                            LDA_modeller(data = ud_raw, 
                                         types = unlist(topic_data$types[x]), 
                                         ntopics = topic_data$ntopics[x])
                          }, mc.cores = 3L)


save(raw_models, topic_data, ntopics, file = "data/LDA_raw_models.RData")
