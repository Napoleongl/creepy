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

episode_names <- episode_namer(episode_titles)

episode_names %<>% mutate(story_count = unlist(sapply(episodes, function(x){ifelse(is.null(nrow(x)),0,nrow(x))})))

#udsv <- udpipe_download_model(language = "swedish")
#udsv <- udpipe_load_model("swedish-talbanken-ud-2.3-181115.udpipe")
udsv <- udpipe_load_model("swedish-talbanken-ud-2.4-190531.udpipe")

episode_data <- bind_rows(episodes) %>% 
  filter(!str_detect(raw_text, "pic\\.twitter\\.com")) %>% 
  mutate(story_words = as.integer(story_words + 1)) %>%  # Eftersom det görs genom att räkna mellansteg 
  mutate(raw_text = utf8::as_utf8(raw_text, normalize = TRUE))


#IS_DONE

# slå ihop 52_2-52_? till en story (breven i kalvstigsamlingen)
# slå ihop ep 109 till en story
# slå ihop 68_2 -68_ till en story (dioneahuset/mailen från Mark)
episode_data %<>% story_merger(episode = "E109") %>% 
  story_merger(episode = "E68", start_id = 2) %>% 
  story_merger(episode = "E52", start_id = 2) 

stoppord <- utf8::as_utf8(unname(unlist(read.csv("stoppord-mycket.csv", stringsAsFactors = FALSE))), normalize = TRUE)

episode_data %<>% mutate(cleaned_text = clean_text(raw_text, stoppord),
                         raw_text = str_remove_all(raw_text, "[“”‘’…´]") ) 

ud_raw <- udpipe_annotate(udsv, parallel.cores = 3L,
                           x = episode_data$raw_text, 
                           doc_id = episode_data$story_id)
ud_raw <- as.data.frame(ud_raw)
ud_raw$topic_level_id <- unique_identifier(ud_raw, fields = c("doc_id", "paragraph_id", "sentence_id"))

ud_clean <- udpipe_annotate(udsv, parallel.cores = 3L,
                          x = episode_data$cleaned_text, 
                          doc_id = episode_data$story_id)
ud_clean <- as.data.frame(ud_clean)
ud_clean$topic_level_id <- unique_identifier(ud_clean, fields = c("doc_id", "paragraph_id", "sentence_id"))

save(ud_raw, ud_clean, file = "annotated_text.RData")
