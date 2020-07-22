suppressPackageStartupMessages(require(rvest))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))



episodes <- list()
try(load("episodes.Rdata"), silent = TRUE)
episode_counter <- length(episodes) + 1L
write(paste("Starting at episode",episode_counter), "")
specials_counter <- 0L
try(load("specials_counter.Rdata"), silent = TRUE)

toc_url <- 'https://www.creepypasta.se/poddmanuskript/'

ep_links <- read_html(toc_url) %>% html_nodes('p a') %>% html_attr('href') 
ep_links <- ep_links[str_detect(ep_links,'creepypasta')]

for(ep in episode_counter:length(ep_links)){
  new_episodes <- story_tibble(ep_links[ep])
  episodes[[ep]] <- new_episodes
 
  save(episodes, file = "episodes.Rdata")
  save(specials_counter, file = "specials_counter.Rdata")
}