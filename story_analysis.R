suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))

source("story_functions.R")
load("episode_download.Rdata")

theme_set(theme_minimal(base_family = "serif") +
            theme(panel.grid.minor = element_blank()))


episode_names <- episode_namer(episode_titles)

episode_names %<>% mutate(story_count = unlist(sapply(episodes, function(x){ifelse(is.null(nrow(x)),0,nrow(x))})))



#udsv <- udpipe_download_model(language = "swedish")
udsv <- udpipe_load_model("swedish-talbanken-ud-2.3-181115.udpipe")

episode_data <- bind_rows(episodes) %>% 
  filter(!str_detect(raw_text, "pic\\.twitter\\.com")) %>% 
  mutate(story_words = as.integer(story_words + 1))


#IS_DONE
# slå ihop 52_2-52_? till en story (breven i kalvstigsamlingen)
# slå ihop ep 109 till en story
# slå ihop 68_2 -68_ till en story (dioneahuset/mailen från Mark)
episode_data %<>% story_merger(episode = "E109") %>% 
  story_merger(episode = "E68", start_id = 2) %>% 
  story_merger(episode = "E52", start_id = 2) 

# https://github.com/peterdalle/svensktext
stoppord <- unname(unlist(read.csv("stoppord-mycket.csv", stringsAsFactors = FALSE)))

episode_data %<>% mutate(cleaned_text = clean_text(raw_text, stoppord))

ud_test1 <- udpipe(x = episode_data$cleaned_text[233], object = udsv)

ud_test <- udpipe_annotate(udsv, x = episode_data$cleaned_text, doc_id = episode_data$story_id)

episode_data %>% group_by(ep_id) %>% 
  summarise(story_count = n()) %>%  
  ggplot() +
  aes(x=story_count) +
  geom_histogram()
  
ggplot(episode_data) +
  aes(x=story_words) +
  geom_histogram(bins = 12) +
  scale_x_sqrt(breaks =c(125,2500,5000,10000,20000,40000,60000)) 

