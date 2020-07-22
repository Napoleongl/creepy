story_extractor <- function(URL){
  read_html(URL) %>% html_nodes('blockquote') %>% html_text() 
}

label_extractor <- function(URL){
  if(!is.character(URL)){ stop("URL is expected to be string!") }
  episode_id  <- str_match(URL, 'avsnitt-[0]?([0-9]+)')[2]
  sub_id <- str_match(URL, '[0-9]+-del-([0-9]+)')[2]
  # Handle special episodes (live-perf etc) withour number
  if(is.na(episode_id)){
    specials_counter <<- specials_counter + 1
    episode_id <- paste0("S",specials_counter)
  }
  if(!is.na(sub_id)){ return(paste0("E", episode_id, ":", sub_id)) }
  return(paste0("E", episode_id))
}


story_tibble <- function(URL){
  Sys.sleep(rpois(1, 10))
  ep_label    <- label_extractor(URL)
  ep_stories  <- story_extractor(URL)
  if(episode_counter %% 5L == 0){ 
    write(paste0(episode_counter, " episodes downloaded."), "") 
  }
  episode_counter <<- episode_counter + 1
  if(length(ep_stories) == 0) {return()}
  names(ep_stories) <- paste0(ep_label, "_S", 1:length(ep_stories))
  ep_data <- enframe(ep_stories, name = "story_id", value = "raw_text")
  ep_data %>% mutate(ep_id = ep_label, 
                     story_no = 1:length(ep_stories),
                     story_chars = nchar(raw_text), 
                     story_words = str_count(raw_text, '[ \\n]+')+1) %>% 
    select(story_id, ep_id, story_no, story_chars, story_words, raw_text)
}

story_merger <- function(.data, episode, start_id = 1, end_id = NULL){
  # Function to merge multiple stories into one
  if(is.null(end_id)){
    story_rows <- .data %>% filter(ep_id == episode &
                                     story_no >= start_id)
  } else {
    story_rows <- .data %>% filter(ep_id == episode &
                                     story_no >= start_id &
                                     story_no <= end_id)
  }
  write(paste(nrow(story_rows), "stories to be merged into one!"), "")
  new_story <- story_rows %>% filter(story_no == start_id) %>% 
    select(story_id, ep_id, story_no) %>% 
    mutate(raw_text = paste(story_rows$raw_text, collapse = "\n"), 
           story_chars = nchar(raw_text),
           story_words = str_count(raw_text, '[ \\n]+')+1) %>% 
    select(story_id, ep_id, story_no, story_words, story_chars, raw_text)
  
  if(is.null(end_id)){
    return(.data %>% filter(ep_id != episode | story_no < start_id) %>% 
             bind_rows(new_story))
  } else {
    return(.data %>% filter(ep_id != episode | story_no < start_id | story_no > end_id) %>% 
             bind_rows(new_story))
  }
}
