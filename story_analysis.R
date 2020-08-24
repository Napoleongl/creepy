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
load("LDA_bigram_models.RData")

model_data <- rbind(topic_data, bigram_data)
models <- c(raw_models, raw_bigram_models)

model_data %<>% mutate(mean_coh = sapply(models, function(x){mean(x$coherence)}),
                       hmean_coh = sapply(models, function(x){1/mean(1/x$coherence)}),
                       gmean_coh = sapply(models, function(x){prod(x$coherence)^(1/length(x$coherence))}),

                       word_types = factor(sapply(types, function(x){paste0(str_to_title(x), 
                                                                            collapse="+")}),
                                           levels = c("Noun", "Adj+Noun", "Verb+Noun",
                                                      "Verb+Adj+Noun", "Noun+Bigrams")))
best_model_idx <- which.max(model_data$mean_coh)

for(model_id in 1:length(models)){
  models[[model_id]]$top_topics <- GetTopTerms(models[[model_id]]$phi, 6)
  models[[model_id]]$prevalence <- colSums(models[[model_id]]$theta)/sum(models[[model_id]]$theta)*100
  }

str(models[[best_model_idx]])
#plot(hclust(as.dist(CalcHellingerDist(models[[best_model_idx]]$phi)),method = "complete"))


models[[18]]$top_terms <- GetTopTerms(models[[18]]$phi, 6)
models[[18]]$prevalence <- colSums(models[[18]]$theta)/sum(models[[18]]$theta)*100
#models[[best_model_idx]]$prevalence



