suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library("ggdendro"))
#Graphs for report
extrafont::loadfonts()
theme_set(theme_minimal(base_family = "Palatino") +
            theme(panel.grid.minor = element_blank()))


# episode_data %>% group_by(ep_id) %>% 
#   summarise(story_count = n()) %>%  
#   ggplot() +
#   aes(x=story_count) +
#   geom_histogram()

story_word_graph <- function(){
  ggsave(filename = "images/story_words_dist.pdf", width = 11, height =4, units ="in",
         plot = 
           ggplot(episode_data) +
           aes(x=story_words) +
           geom_histogram(bins = 12, fill = word_type_colours[1], 
                          colour = word_type_colours[length(word_type_colours)]) +
           scale_x_sqrt("Number of words", breaks =c(250,2000,5000,10000,20000,40000,60000)) +
           labs(title = "Distribution of story length in number of words",
                y = "Number of stories")
  )
}

example_data <- function(){
  # Example data for data chapter
  set.seed(141114)
  episode_data[sample(1:543,5),c(1,5,6)] %>% 
    mutate(raw_text = paste0(substr(raw_text,1,200),"...")) %>% 
    xtable(digits =0,caption = "Example of episode data prior to annotation. Only the first 200 characters of the raw\\_text-field are shown") %>% 
    print.xtable(booktabs = TRUE, caption.placement = "top")
}

model_comp_graph <- function(){
  # Plot for comparing models
  word_type_colours <- RColorBrewer::brewer.pal(length(levels(model_data$word_types))+1, "RdGy")[-c(4)]
  names(word_type_colours) <- levels(model_data$word_types)
  
  ggsave(filename = "images/model_comp.pdf", width = 10, height =4, units ="in",
         plot = 
           ggplot(model_data) + 
           aes(x=ntopics, y=mean_coh, fill=word_types, colour= word_types, shape = word_types) +
           geom_point(data = model_data[which.max(model_data$mean_coh),], color = "#33333330", 
                      fill = "#33333330", shape = 21, size = rel(6)) +
           geom_line(size = rel(0.75)) +
           geom_point(size = rel(2), colour = "grey20") +
           scale_shape_manual("POS-tag",values = 21:25) +
           scale_fill_manual("POS-tag", values = word_type_colours, aesthetics = c("fill", "colour"))+
           scale_y_continuous("Average coherence")+
           scale_x_continuous("Number of topics", breaks = ntopics[c(T,F)])+
           ggtitle("Average topic coherence by number of topics and POS-tags included") +
           theme (legend.position = "right") 
  )
}

best_model_graph <- function(){
  # GRaph of topics and coherence
  word_type_colours <- RColorBrewer::brewer.pal(length(levels(model_data$word_types))+1, "RdGy")[-c(4)]
  bar_cols = setNames(c(word_type_colours[1], word_type_colours[3], word_type_colours[5]),
                      c("coherence", "prevalence", "outl"))
  topic_plot_height <- max(model_data[best_model_idx, "ntopics"]/4+2, 4.5)
  topic_similarity <-lsa::cosine(t(models[[best_model_idx]]$phi)) 
  topic_clust <- hclust(as.dist(1-topic_similarity), method = "ward.D")
  topic_dendro <- as.dendrogram(topic_clust)
  topic_dend_order <- order.dendrogram(topic_dendro)
  topic_tiles <- data.frame(xmax = c(5.5,9.5,12.5,18.42), xmin = c(0.6,5.5,9.5,12.5),
                            ymin = -2.9, ymax = 1.05, cluster = letters[1:4])
  topic_dendrogram <- ggdendrogram(topic_dendro, rotate=TRUE, labels = FALSE) +
  geom_rect(data = topic_tiles, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                                    fill=cluster, colour = cluster), size = rel(0.2)) +
    scale_fill_manual(values = alpha(RColorBrewer::brewer.pal(4,"RdGy"),.25)) +
    scale_colour_manual(values = alpha(RColorBrewer::brewer.pal(4,"RdGy"),1)) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")
  
  topic_graph <- tibble(coherence  = models[[best_model_idx]]$coherence,
                        prevalence = models[[best_model_idx]]$prevalence/100,
                        topic_text = apply(models[[best_model_idx]]$top_topics, 2 ,paste, collapse = ", "),
                        topic_id   = colnames(models[[best_model_idx]]$top_topics),
                        topic_label= paste0("{", topic_text, "}=",topic_id)) %>%
    mutate(topic_label = factor(topic_label, levels = topic_label[topic_dend_order], ordered = TRUE)) %>% 
    pivot_longer(-starts_with("topic"), names_to = "measure", values_to = "value") %>% 
    ggplot()+
    aes(y=topic_label, x=value, group=measure, fill = measure) +
    geom_col(width = rel(0.15), size = rel(0.1), colour = bar_cols["outl"],position=position_dodge(.55)) +
    geom_point(size = rel(2),shape = 22, colour = bar_cols["outl"],position=position_dodge(.55)) +
    labs(x="", title = "")+
    scale_x_reverse("", expand = c(0,0.01)) + 
    scale_y_discrete("", position = "right")+
    scale_fill_manual("",values = bar_cols) +
    theme(legend.position = "bottom", axis.text.y.right = element_text(hjust = 0.)) 
  pdf("images/toptopics.pdf", height = 5.2, width = 11)
  print(topic_dendrogram, vp = grid::viewport(x = 0.75, y = 0.54, width = 0.5, height = 0.89))
  print(topic_graph, vp=grid::viewport(x = 0.42, y = 0.5, width = 0.8, height = 1.0))
  dev.off()
 }
