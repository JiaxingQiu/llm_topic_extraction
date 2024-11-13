setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

# example: 104a474

rm(list=ls())

library(dplyr)
fea_df <- read.csv("./data/fea_df.csv")
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE) %>% select(sm_id, group, sr_name, url)
labeled_df_gpt4o <- read.csv("./gpt4o_data/results.csv", stringsAsFactors = F)
labeled_df_gpt4o <- merge(labeled_df_gpt4o, info_df, by = "sm_id", all.x = TRUE)


# filter subreddit that have at least 10 posts
subset_sr <- labeled_df_gpt4o %>% group_by(sr_name) %>%
  summarise(nsm = n_distinct(sm_id)) %>%
  filter(nsm >= 10) %>%
  as.data.frame()


# In dataframe labeled_df, group by sr_name and sample 10 posts per sr_name
set.seed(333)
sampled_df <- labeled_df_gpt4o %>%
  filter(sr_name %in% c(subset_sr$sr_name) ) %>%
  group_by(sr_name) %>%
  sample_n(size = 10) %>%
  ungroup()  


sampled_df_human <- sampled_df[,c("sm_id", "text_w_eos", "answer_string")]
sampled_df_human$answer_string_human <- paste0(paste0(paste0("(",1:length(fea_df$fea),") ", fea_df$fea), collapse = ": 0\n"), ": 0\n")
write.csv(sampled_df_human, "./human_data/sampled_posts.csv", row.names = F)

