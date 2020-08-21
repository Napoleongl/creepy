suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(topicmodels))
suppressPackageStartupMessages(require(parallel))
suppressPackageStartupMessages(require(textmineR))

source("story_functions.R")
load("LDA_raw_models.RData")
#load(file = "LDA_raw_models2.RData")

model_data <- rbind(topic_data, bigram_data)
models <- c(raw_models, raw_bigram_models)

model_data %<>% mutate(mean_coh = sapply(models, function(x){mean(x$coherence)}),
                       hmean_coh = sapply(models, function(x){1/mean(1/x$coherence)}),
                       gmean_coh = sapply(models, function(x){prod(x$coherence)^(1/length(x$coherence))}),
                       word_types = factor(sapply(types, function(x){paste0(str_to_title(x), 
                                                                            collapse="+")}),
                                           levels = c("Noun", "Adj+Noun", "Verb+Noun",
                                                      "Verb+Adj+Noun", "Noun+Bigrams")))
top_topics <- sapply(models, function(model){
  GetTopTerms(model$phi, 8)
})


