suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(topicmodels))
suppressPackageStartupMessages(require(parallel))
suppressPackageStartupMessages(require(textmineR))
suppressPackageStartupMessages(require(xtable))

source("R/story_functions.R")
source("R/story_graphs.R")

load("data/LDA_raw_models.RData")
load("data/LDA_bigram_models.RData")

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

model_comp_graph()

best_model_graph()


story_similarity <-lsa::cosine(t(models[[best_model_idx]]$theta)) 
  story_similarity %>%  matrix_to_named_vector() %>% 
    enframe("stories", "similarity") -> similarity_df

story_outliers <- colSums(story_similarity) %>% enframe("story", "similarity_sum") %>% 
  arrange((similarity_sum))
  
#Least/Most Similar
similarity_df %>% 
  arrange(desc(similarity)) %>% 
  head(10) %>% 
  xtable(digits=4) %>% 
  print(include.rownames=FALSE)

similarity_df %>% 
  arrange((similarity)) %>% 
  head(10) %>% 
  xtable(digits=4) %>% 
  print(include.rownames=FALSE)

#Story Clustering
story_clust <- hclust(as.dist(1-story_similarity), method = "ward.D2")
story_dendro <- as.dendrogram(story_clust)
story_dendrogram <- ggdendrogram(story_dendro, rotate=TRUE) 

load(file="data/episode_data.RData")


story_pair("E11_S3-E13_S1")
