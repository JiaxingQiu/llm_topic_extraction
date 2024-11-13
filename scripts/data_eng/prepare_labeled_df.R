setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

# example: 104a474

rm(list=ls())

library(dplyr)
library(parallel)
source("./scripts/data_eng/func_label_topics.R")
source("./scripts/data_eng/func_extract_phrases.R")
source("./scripts/data_eng/func_plot_distribution.R")
fea_df <- read.csv("./data/fea_df.csv")



# ------ 0. human --------
if(!file.exists("./human_data/results.csv")){
  # loop through all csv files under "./gpt_data" folder
  folder_path <- "./human_data/"
  
  label1 <- read.csv(paste0(folder_path,"sampled_posts_joy.csv"))
  label2 <- read.csv(paste0(folder_path,"sampled_posts_sophia.csv"))
  
  label1 <- label1[,c("sm_id", "answer_string")]
  label2 <- label1[,c("sm_id", "answer_string")]
  
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  answer_df$answer_string <- answer_df$responses
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_gpt4omini <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_gpt4omini, "./gpt_data/results.csv", row.names = F)
}else{
  labeled_df_gpt4omini <- read.csv("./gpt_data/results.csv", stringsAsFactors = F)
}



# ------ 1. GPT 4o mini --------
if(!file.exists("./gpt_data/results.csv")){
  # loop through all csv files under "./gpt_data" folder
  folder_path <- "./gpt_data"
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  answer_df$answer_string <- answer_df$responses
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_gpt4omini <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_gpt4omini, "./gpt_data/results.csv", row.names = F)
}else{
  labeled_df_gpt4omini <- read.csv("./gpt_data/results.csv", stringsAsFactors = F)
}




# ------ 2. llama 8b -------
if(!file.exists("./llama_data/results.csv")){
  # # answer_df_part1.csv contain first part of llama query results
  # # part 1
  # answer_df_llama1 <- read.csv("./llama_data/answer_df_part1.csv", stringsAsFactors = F)
  # answer_df_llama1$answer_string <- gsub("thinspo","thinny",answer_df_llama1$answer_string)
  # answer_df_llama1$answer_string <- gsub("bodyhate","body",answer_df_llama1$answer_string)
  # answer_df <- answer_df_llama1 %>% filter( (!is.na(answer_string)) & (!answer_string=="") &
  #                                             (!is.na(answer_string2)) & (!answer_string2=="") &
  #                                             (!is.na(answer_string3)) & (!answer_string3=="") ) %>%
  #   as.data.frame()
  # answer_df$answer_string <- paste0(answer_df$answer_string, "\n", answer_df$answer_string2, "\n", answer_df$answer_string3 )
  # answer_df <- answer_df %>% select(sm_id, text_w_eos, answer_string) %>% as.data.frame()
  
  # new part 1
  answer_df_llama1 <- read.csv("./llama_data/answer_df_part3.csv", stringsAsFactors = F)
  # part 2
  answer_df_llama2 <- read.csv("./llama_data/answer_df_part2.csv", stringsAsFactors = F)
  answer_df <- bind_rows(answer_df_llama1, answer_df_llama2)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_llama8b <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_llama8b, "./llama_data/results.csv", row.names = F)
}else{
  labeled_df_llama8b <- read.csv("./llama_data/results.csv", stringsAsFactors = F)
}


# ------ 3. GPT 4o --------
if(!file.exists("./gpt4o_data/results.csv")){
  # loop through all csv files under "./gpt4o_data" folder
  folder_path <- "./gpt4o_data"
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  answer_df$answer_string <- answer_df$responses
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_gpt4o <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_gpt4o, "./gpt4o_data/results.csv", row.names = F)
}else{
  labeled_df_gpt4o <- read.csv("./gpt4o_data/results.csv", stringsAsFactors = F)
}





# ------ 3. viz distribution ------

# additional info
text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
info_df <- text_df %>% select(sm_id, group, sr_name, url)
labeled_df_gpt4o <- merge(labeled_df_gpt4o, info_df, by = "sm_id", all.x = TRUE)
labeled_df_gpt4omini <- merge(labeled_df_gpt4omini, info_df, by = "sm_id", all.x = TRUE)
labeled_df_llama8b <- merge(labeled_df_llama8b, info_df, by = "sm_id", all.x = TRUE)
rm(text_df)

# sanity check
plot_distribution(labeled_df_gpt4omini,
                  labeled_df_llama8b,
                  labeled_df_gpt4o)

# # ------- 4. sample human label data --------
# labeled_df_gpt4omini <- merge(labeled_df_gpt4omini, labeled_df_llama8b[,c("sm_id","text_w_eos")])
# tmp <- labeled_df_gpt4omini %>% 
#   group_by(sr_name) %>% 
#   summarise(nsm = n_distinct(sm_id)) %>%
#   filter(nsm>=10) %>% as.data.frame() # 
# labeled_df <- labeled_df_gpt4omini %>% filter(sr_name %in% tmp$sr_name)
# print(setdiff(unique(labeled_df_gpt4omini$sr_name),tmp$sr_name)) # remember to remove these forums from semantic network
# set.seed(333)
# sampled_df <- labeled_df %>%
#   group_by(sr_name) %>%
#   sample_n(size = 10) %>%
#   ungroup()  # Remove grouping for further analysis
# print(sampled_df$sm_id)
# sampled_df_human <- sampled_df[,c("sm_id","text_w_eos","answer_string")]
# sampled_df_human$answer_string_human <- paste0(paste0(fea_df$fea, collapse = ": 0\n"), ": 0\n")
# write.csv(sampled_df_human, "human_data/sampled_posts.csv", row.names = F)


