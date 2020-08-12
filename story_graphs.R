#Graphs for report
theme_set(theme_minimal(base_family = "serif") +
            theme(panel.grid.minor = element_blank()))


episode_data %>% group_by(ep_id) %>% 
  summarise(story_count = n()) %>%  
  ggplot() +
  aes(x=story_count) +
  geom_histogram()

ggplot(episode_data) +
  aes(x=story_words) +
  geom_histogram(bins = 12) +
  scale_x_sqrt(breaks =c(125,2500,5000,10000,20000,40000,60000)) 

