suppressPackageStartupMessages(library(patchwork))
#Graphs for report
theme_set(theme_minimal(base_family = "Palatino") +
            theme(panel.grid.minor = element_blank()))


episode_data %>% group_by(ep_id) %>% 
  summarise(story_count = n()) %>%  
  ggplot() +
  aes(x=story_count) +
  geom_histogram()

ggsave(filename = "images/story_words_dist.pdf", width = 11, height =4, units ="in",
       plot = 
ggplot(episode_data) +
  aes(x=story_words) +
  geom_histogram(bins = 12, fill = word_type_colours[1], colour = word_type_colours[length(word_type_colours)]) +
  scale_x_sqrt("Number of words", breaks =c(150,2000,5000,10000,20000,40000,60000)) +
  labs(y = "Number of stories", title = "Distribution of story length in number of words")
)


# Plot for comparing models
word_type_colours <- RColorBrewer::brewer.pal(length(levels(model_data$word_types))+1, "RdGy")[-c(4)]
names(word_type_colours) <- levels(model_data$word_types)

ggsave(filename = "images/model_comp.pdf", width = 11, height =4.5, units ="in",
       plot = 
         ggplot(model_data) + 
         aes(x=ntopics, y=mean_coh, fill=word_types, colour= word_types, shape = word_types) +
         geom_point(data = model_data[which.max(model_data$mean_coh),], color = "#33333330", fill = "#33333330", shape = 21, size = rel(6)) +
         geom_line(size = rel(0.75)) +
         geom_point(size = rel(2), colour = "grey20") +
         scale_shape_manual("Word types",values = 21:25) +
         scale_fill_manual("Word types", values = word_type_colours, aesthetics = c("fill", "colour"))+
         scale_y_continuous("Average coherence")+
         scale_x_continuous("Number of topics", breaks = ntopics[c(T,F)])+
         ggtitle("Average topic coherence by number of topics and word types included") +
         theme (legend.position = "right") 
)

