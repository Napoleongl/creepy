suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(xtable))
#Graphs for report
extrafont::loadfonts()
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
  scale_x_sqrt("Number of words", breaks =c(250,2000,5000,10000,20000,40000,60000)) +
  labs(y = "Number of stories", title = "Distribution of story length in number of words")
)


# Example data for data chapter
set.seed(141114)
episode_data[sample(1:543,5),c(1,5,6)] %>% 
  mutate(raw_text = paste0(substr(raw_text,1,200),"...")) %>% 
  xtable(digits =0,caption = "Example of episode data prior to annotation. Only the first 200 characters of the raw\\_text-field are shown") %>% 
  print.xtable(booktabs = TRUE, caption.placement = "top")



# Plot for comparing models
word_type_colours <- RColorBrewer::brewer.pal(length(levels(model_data$word_types))+1, "RdGy")[-c(4)]
names(word_type_colours) <- levels(model_data$word_types)

ggsave(filename = "images/model_comp.pdf", width = 10, height =4, units ="in",
       plot = 
         ggplot(model_data) + 
         aes(x=ntopics, y=mean_coh, fill=word_types, colour= word_types, shape = word_types) +
         geom_point(data = model_data[which.max(model_data$mean_coh),], color = "#33333330", fill = "#33333330", shape = 21, size = rel(6)) +
         geom_line(size = rel(0.75)) +
         geom_point(size = rel(2), colour = "grey20") +
         scale_shape_manual("POS-tag",values = 21:25) +
         scale_fill_manual("POS-tag", values = word_type_colours, aesthetics = c("fill", "colour"))+
         scale_y_continuous("Average coherence")+
         scale_x_continuous("Number of topics", breaks = ntopics[c(T,F)])+
         ggtitle("Average topic coherence by number of topics and POS-tags included") +
         theme (legend.position = "right") 
)

# GRaph of topics and coherence
bar_cols = setNames(c(word_type_colours[1], word_type_colours[3], word_type_colours[5]),
                    c("coherence", "prevalence", "outl"))
topic_plot_height <- max(model_data[best_model_idx, "ntopics"]/4+2, 4.5)
ggsave(filename = "images/toptopics.pdf", width = 10, height = topic_plot_height, units ="in",
       plot =
         tibble(coherence = models[[best_model_idx]]$coherence,
                prevalence = models[[best_model_idx]]$prevalence/100,
                topic_text = apply(top_topics[[best_model_idx]], 2 ,paste, collapse = ", ")) %>%
         pivot_longer(-topic_text, names_to = "measure", values_to = "value") %>% 
         
         ggplot()+
         aes(x=topic_text, y=value, group=measure, fill = measure) +
         geom_col(width = rel(0.15), size = rel(0.1), colour = bar_cols["outl"],position=position_dodge(.55)) +
         geom_point(size = rel(2),shape = 22, colour = bar_cols["outl"],position=position_dodge(.55)) +
         labs(x="", title = "Top terms, coherence and prevalence per topic")+
         scale_y_continuous("", expand = c(0,0.01)) + 
         scale_fill_manual("",values = bar_cols) +
         theme(legend.position = "bottom") +
         coord_flip()
)
