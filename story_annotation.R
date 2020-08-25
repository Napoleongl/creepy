#Script for annotating (lemmatization, POS-tagging) downloaded stories. Also combines
#some story-rows in data that come from the same story but are split into multiple 
#blockquotes. Saves both a cleaned up version and the raw texts.

suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(topicmodels))
suppressPackageStartupMessages(require(parallel))
suppressPackageStartupMessages(require(textmineR))

source("story_functions.R")
load("episode_download.Rdata")

udsv <- udpipe_load_model("swedish-talbanken-ud-2.4-190531.udpipe")

# Cleaning out duplicate stories, and merging multi-blockquote-stories
episode_data <- bind_rows(episodes) %>% 
  filter(!str_detect(raw_text, "pic\\.twitter\\.com")) %>% 
  mutate(story_words = as.integer(story_words + 1)) %>%  # Eftersom det görs genom att räkna mellansteg 
  mutate(raw_text = utf8::as_utf8(raw_text, normalize = TRUE)) %>% 
  story_merger(episode = "E52", start_id = 2) %>% #Kalvstigssamlingen
  story_merger(episode = "E68", start_id = 2) %>% #Dioneahuset
  story_merger(episode = "E80") %>% #luckan på vinden
  story_merger(episode = "E93", start_id = 2, end_id = 3) %>% # Osbystranden
  story_merger(episode = "E100:1", start_id = 1) %>% #HOIN
  story_merger(episode = "E100:2", start_id = 1) %>% #HOIN
  story_merger(episode = "E100:3", start_id = 1) %>% #HOIN
  story_merger(episode = "E100:4", start_id = 1) %>% #HOIN
  story_merger(episode = "E100:5", start_id = 1) %>% #HOIN
  story_merger(episode = "E109") %>%  #Twitterhistorien
  story_merger(episode = "E116", start_id = 2, end_id = 4) %>% # Tredelad historia om besökare
  story_merger(episode = "E116", start_id = 6, end_id = 9) %>% # Cooly om övervakning
  story_merger(episode = "E118", start_id = 4, end_id = 7) %>% # SCP-106
  story_merger(episode = "E130", start_id = 3) %>%  #den löånga flygresan i fyra delar
  filter(story_words > 30) %>% 
  filter(!(story_id %in% c("E89_S2", "E67_S8"))) #Mailsvar samt dublett
write(paste(nrow(episode_data),"episodes to annotate."),"")
save(episode_data, file="episode_data.RData")
stoppord <- utf8::as_utf8(unname(unlist(read.csv("stoppord-mycket.csv", 
                                                 stringsAsFactors = FALSE))), 
                          normalize = TRUE)

episode_data %<>% mutate(raw_text = utf8::as_utf8(str_remove_all(raw_text, "[“”‘’…´]"),TRUE)) 

story_word_graph()

ud_raw <- udpipe_annotate(udsv, parallel.cores = 3L,
                           x = episode_data$raw_text, 
                           doc_id = episode_data$story_id)
ud_raw <- as.data.frame(ud_raw)
ud_raw$topic_level_id <- unique_identifier(ud_raw, fields = c("doc_id", "paragraph_id", "sentence_id"))

save(ud_raw, file = "annotated_text.RData")
