# Repository for textmining project

## Topic Analysis and clustering of creepypasta in Swedish

This repository contains the code used to complete a project work as part of the course [732A92 - Text mining](https://www.ida.liu.se/~732A92/) at Linköping University.

The code is organised into a number of scripts designed to be run in sequence, all of which are found under the `R/`-directory.

1. **story_getter.R** Downloads all the episode stories and saves them into `data/episode_download.Rdata`.
2. **story_annotation.R** Gathers story observations into documents and annotates them with part-of-speech-tags (POS-tags) and lemmatized form. The data is saved to `data/annotated_text.RData`.
3. **raw_topicmodelling.R** and  **bigram_topicmodelling.R** models the actual topics. They are only split into two files in order to be parallized efficently on the machine used to do the modelling. The list of models on different POS-tags and number of topics are saved to `data/LDA_raw_models.RData` and `data/LDA_bigram_models.RData` respectively.
4. **story_analysis.R** gathers the models and produces graphs and LaTeX-tables used in the report.

The files **story_functions.R** and  **story_graphs.R** contains various helper functions created to ensure that the modelling-files are kept somewhat clean and readable.

The abstract of the project report is found [here]()