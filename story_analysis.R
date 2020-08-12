suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(topicmodels))

source("story_functions.R")
load("episode_download.Rdata")

episode_names <- episode_namer(episode_titles)

episode_names %<>% mutate(story_count = unlist(sapply(episodes, function(x){ifelse(is.null(nrow(x)),0,nrow(x))})))

#udsv <- udpipe_download_model(language = "swedish")
udsv <- udpipe_load_model("swedish-talbanken-ud-2.3-181115.udpipe")

episode_data <- bind_rows(episodes) %>% 
  filter(!str_detect(raw_text, "pic\\.twitter\\.com")) %>% 
  mutate(story_words = as.integer(story_words + 1)) # Eftersom det görs genom att räkna mellansteg 


#IS_DONE

# slå ihop 52_2-52_? till en story (breven i kalvstigsamlingen)
# slå ihop ep 109 till en story
# slå ihop 68_2 -68_ till en story (dioneahuset/mailen från Mark)
episode_data %<>% story_merger(episode = "E109") %>% 
  story_merger(episode = "E68", start_id = 2) %>% 
  story_merger(episode = "E52", start_id = 2) 

# https://github.com/peterdalle/svensktext
stoppord <- unname(unlist(read.csv("stoppord-mycket.csv", stringsAsFactors = FALSE)))

episode_data %<>% mutate(#cleaned_text = clean_text(raw_text, stoppord),
                         raw_text = str_remove_all(raw_text, "[“’”]")) 

#ud_test1 <- udpipe(x = episode_data$cleaned_text[233], object = udsv)

ud_test <- udpipe_annotate(udsv, parallel.cores = 3L,
                           x = episode_data$raw_text, 
                           doc_id = episode_data$story_id)
ud_test <- as.data.frame(ud_test)
str(ud_test)
save(ud_test, file = "annotated_raw_text.RData")
load(file = "annotated_raw_text.RData")
ud_test$topic_level_id <- unique_identifier(ud_test, fields = c("doc_id", "paragraph_id", "sentence_id"))
dtf <- subset(ud_test, upos %in% c("ADJ","NOUN", "VERB"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
head(dtf)

## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 3)
dtm_clean <- dtm_remove_terms(dtm_clean, terms = stoppord)
head(dtm_colsums(dtm_clean))

m <- LDA(dtm_clean, k = 10, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

predict(m, type = "terms", min_posterior = 0.05, min_terms = 6)
