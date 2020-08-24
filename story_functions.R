story_extractor <- function(episode_page){
  episode_page %>% html_nodes('blockquote') %>% html_text() 
}

episode_name_extractor <- function(episode_page){
  episode_page %>% html_nodes('.entry-title') %>% html_text() 
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


story_tibble <- function(episode_page, episode_label){
  ep_stories  <- story_extractor(episode_page)
  if(length(ep_stories) == 0) {return()}
  names(ep_stories) <- paste0(episode_label, "_S", 1:length(ep_stories))
  ep_data <- enframe(ep_stories, name = "story_id", value = "raw_text")
  ep_data %>% mutate(ep_id = episode_label, 
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

clean_text <- function(text, stopword){
  text %>% tolower() %>% 
    str_remove_all(pattern = "[“”‘’…´]") %>% 
    tm::removePunctuation(preserve_intra_word_dashes = FALSE) %>% 
    tm::removeWords(stopword) %>% 
    tm::removeNumbers()
  #tm::removePunctuation(tm::removeNumbers(tm::removeWords(tolower(text), stopword)), preserve_intra_word_dashes = FALSE)
}

episode_namer <- function(page_title){
  ep_name <- str_match(page_title, "(Avsnitt )?([:print:]*)\\:\\s([:print:]*)")
  as_tibble(ep_name[, 3:4])
}


LDA_modeller <- function(data, types, ntopics){
  # https://github.com/peterdalle/svensktext
  stoppord <- utf8::as_utf8(unname(unlist(read.csv("stoppord-mycket.csv", stringsAsFactors = FALSE))), normalize = TRUE)
  
  dtf <- subset(data, upos %in% types)
  dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")

  ## Create a document/term/matrix for building a topic model
  dtm <- document_term_matrix(x = dtf)
  ## Remove words which do not occur that much and stop words
  dtm_clean <- dtm_remove_lowfreq(dtm_remove_terms(dtm, terms = stoppord), minfreq = 5)
  write(paste("Modelling", paste0(types,collapse = ","),",",ntopics, "topics"),"")
  FitLdaModel(dtm_clean, k = ntopics, iterations = 2500, burnin = 800, 
              calc_coherence = TRUE,calc_r2 = TRUE, calc_likelihood=TRUE)
}


LDA_ngram_modeller <- function(data, types, ntopics, ngramtypes = c("NOUN", "ADJ")){
  ## Find keywords with RAKE 
  keyw_rake <- keywords_rake(data, 
                             term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), 
                             relevant = data$upos %in% ngramtypes, 
                             ngram_max = 2, n_min = 3)
  ## Find simple noun phrases
  data$phrase_tag <- as_phrasemachine(data$upos, type = "upos")
  keyw_nounphrases <- keywords_phrases(data$phrase_tag, term = data$token, 
                                       pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, 
                                       detailed = FALSE)
  keyw_nounphrases <- subset(keyw_nounphrases, ngram > 1)
  
  ## Recode terms to keywords
  data$term <- data$token
  ## Recode tokens to keywords, if it is not in the list of tokens, set to NA
  ngrams <- txt_recode_ngram(data$token, compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
  data$mwe <- ngrams
  data$mwe <- ifelse(data$mwe %in% keyw_rake$keyword, data$mwe, NA)
  
  ## nouns
  data$term_type <- ifelse(data$upos %in% types, data$lemma, NA)
  # https://github.com/peterdalle/svensktext
  stoppord <- utf8::as_utf8(unname(unlist(read.csv("stoppord-mycket.csv", stringsAsFactors = FALSE))), normalize = TRUE)
  
  ## Build document/term/matrix 
  dtm <- document_term_frequencies(data, document = "doc_id", term = c("term_type", "mwe"))
  dtm <- document_term_matrix(x = dtm)
  dtm_clean <- dtm_remove_lowfreq(dtm_remove_terms(dtm, terms = stoppord), minfreq = 5)
  write(paste("Modelling", paste0(types,collapse = ","),",",ntopics, "topics"),"")
  FitLdaModel(dtm_clean, k = ntopics, iterations = 2000, burnin = 700, 
                       calc_coherence  = TRUE,calc_r2 = TRUE, calc_likelihood=TRUE)
}
