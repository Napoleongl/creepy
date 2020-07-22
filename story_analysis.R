suppressPackageStartupMessages(require(udpipe))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(tm))

load("episodes.Rdata")

theme_set(theme_minimal(base_family = "serif") +
            theme(panel.grid.minor = element_blank()))

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

clean_text <- function(text, stopword){
  tm::removePunctuation(tm::removeNumbers(tm::removeWords(tolower(text), stopword)))
}

episode_data %<>% mutate(cleaned_text = clean_text(raw_text, stoppord))

episode_data %>% group_by(ep_id) %>% 
  summarise(story_count = n()) %>%  
  ggplot() +
  aes(x=story_count) +
  geom_histogram()
  
ggplot(episode_data) +
  aes(x=story_words) +
  geom_histogram(bins = 12) +
  scale_x_sqrt(breaks =c(125,2500,5000,10000,20000,40000,60000)) 

udsv <- udpipe_download_model(language = "swedish")

ud_test1 <- udpipe(x = episode_data$cleaned_text[1], object = udsv)

tm::stopwords("sv")
tm::removeWords()