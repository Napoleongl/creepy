#Script that downloads episodes from creepypasta.se, extracts each separate story
#(by blockquote-tag) and saves it to a data.frame with one row per story. 
#Can be restarted to download new stories or if it was abrubted. 
#Implements a 10 second (roughly) pause between each pageview to honor scraping reqs.

suppressPackageStartupMessages(require(rvest))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyverse))

source("R/story_functions.R")

toc_url    <- 'https://www.creepypasta.se/poddmanuskript/'

ep_links   <- read_html(toc_url) %>% html_nodes('p a') %>% html_attr('href') 
ep_links   <- ep_links[str_detect(ep_links,'creepypasta')]
ep_count   <- length(ep_links)

episodes   <- list()
episode_titles   <- vector("character", ep_count)
episode_counter  <- 0L
specials_counter <- 0L

try(load("data/episode_download.Rdata"), silent = TRUE)

ep_pb      <- txtProgressBar(max=ep_count, style = 3)
write(paste("Starting at episode",episode_counter), "")
for(ep in (episode_counter+1):ep_count){
  Sys.sleep(rpois(1, 10))
  episode_page    <- read_html(ep_links[ep])
  
  setTxtProgressBar(ep_pb, episode_counter)
  episode_label   <- label_extractor(ep_links[ep])
  episodes[[ep]]  <- story_tibble(episode_page, episode_label)
  episode_titles[ep] <- episode_name_extractor(episode_page)
  episode_counter <- episode_counter + 1
  save(episodes, episode_titles, episode_counter, specials_counter,
       file = "data/episode_download.Rdata")
}
close(ep_pb)