setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

# example: 104a474

rm(list=ls())

library(dplyr)
library(parallel)


path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
fea_df <- read.csv("./data/fea_df.csv")



# ------ 0. human --------
if(!file.exists("./human_data/results.csv")){
  
  library(readxl)
  df_human1 <- read_excel("./human_data/sampled_posts_joy.xlsx", sheet = "joy")
  df_human1 <- df_human1 %>% arrange(sm_id)
  parse_answers <- function(answer_str) {
    # Extract all key-value pairs using regular expression, ignoring the initial numbering
    pairs <- str_extract_all(answer_str, "\\(\\d+\\)\\s(\\w+):\\s(\\d)")[[1]]
    # Split the pairs into topics and values
    split_pairs <- str_split(pairs, ":\\s")
    # Convert to a named vector, removing the numbering
    values <- sapply(split_pairs, function(x) as.integer(x[2]))
    names(values) <- sapply(split_pairs, function(x) {
      # Remove the initial numbering from the topic name
      sub("^\\(\\d+\\)\\s", "", x[1])
    })
    return(values)
  }
  # Assuming df_human1 has columns 'sm_id', 'text_w_eos', and 'answer_string_human'
  answer_matrix <- t(apply(df_human1, 1, function(x) parse_answers(x['answer_string_human'])))
  # Convert the matrix to a dataframe
  answer_df <- as.data.frame(answer_matrix)
  labeled_df_human <- cbind(df_human1[c("sm_id", "text_w_eos")], answer_df)
  labeled_df_human$idealbody <- ifelse(labeled_df_human$thinspo + labeled_df_human$leanbody > 0, 1, 0)
  labeled_df_human$fearfood <- ifelse(labeled_df_human$fearfood + labeled_df_human$fearcarb > 0, 1, 0)
  
  write.csv(labeled_df_human, "./human_data/results.csv", row.names = F)
}else{
  labeled_df_human <- read.csv("./human_data/results.csv", stringsAsFactors = F)
}
if(!file.exists("./human_data/results2.csv")){
  
  library(readxl)
  df_human2 <- read_excel("./human_data/sampled_posts_dongliang.xlsx", sheet = "dongliang")
  df_human2 <- df_human2 %>% arrange(sm_id)
  parse_answers <- function(answer_str) {
    # Extract all key-value pairs using regular expression, ignoring the initial numbering
    pairs <- str_extract_all(answer_str, "\\(\\d+\\)\\s(\\w+):\\s(\\d)")[[1]]
    # Split the pairs into topics and values
    split_pairs <- str_split(pairs, ":\\s")
    # Convert to a named vector, removing the numbering
    values <- sapply(split_pairs, function(x) as.integer(x[2]))
    names(values) <- sapply(split_pairs, function(x) {
      # Remove the initial numbering from the topic name
      sub("^\\(\\d+\\)\\s", "", x[1])
    })
    return(values)
  }
  # Assuming df_human1 has columns 'sm_id', 'text_w_eos', and 'answer_string_human'
  answer_matrix <- t(apply(df_human2, 1, function(x) parse_answers(x['answer_string_human'])))
  # Convert the matrix to a dataframe
  answer_df <- as.data.frame(answer_matrix)
  labeled_df_human <- cbind(df_human2[c("sm_id", "text_w_eos")], answer_df)
  labeled_df_human$idealbody <- ifelse(labeled_df_human$thinspo + labeled_df_human$leanbody > 0, 1, 0)
  labeled_df_human$fearfood <- ifelse(labeled_df_human$fearfood + labeled_df_human$fearcarb > 0, 1, 0)
  
  write.csv(labeled_df_human, "./human_data/results2.csv", row.names = F)
}else{
  labeled_df_human2 <- read.csv("./human_data/results2.csv", stringsAsFactors = F)
}


# ------ 1. GPT 4o mini --------
if(!file.exists("./gpt_data/results.csv")){
  # loop through all csv files under "./gpt_data" folder
  folder_path <- "./gpt_data"
  csv_files <- list.files(path = folder_path, pattern = "output[0-9]+\\.csv" , full.names = TRUE) #"*.csv"
  stopifnot(length(csv_files)==80)
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
  csv_files <- list.files(path = folder_path, pattern = "output[0-9]+\\.csv", full.names = TRUE)
  stopifnot(length(csv_files)==11)
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


# ------ 4. Qwen --------
if(!file.exists("./qwen_data/results.csv")){
  # loop through all csv files under "./gpt4o_data" folder
  folder_path <- "./qwen_data"
  csv_files <- list.files(path = folder_path, pattern = "answer_df_part[0-9]+\\.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_qwen <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_qwen, "./qwen_data/results.csv", row.names = F)
}else{
  labeled_df_qwen <- read.csv("./qwen_data/results.csv", stringsAsFactors = F)
}


# ------ 5. Vicuna13b --------
if(!file.exists("./vicuna13b_data/results.csv")){
  # loop through all csv files under "./gpt4o_data" folder
  folder_path <- "./vicuna13b_data"
  csv_files <- list.files(path = folder_path, pattern = "answer_df_part[0-9]+\\.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_vicuna13b <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_vicuna13b, "./vicuna13b_data/results.csv", row.names = F)
}else{
  labeled_df_vicuna13b <- read.csv("./vicuna13b_data/results.csv", stringsAsFactors = F)
}

# ------ 6. Vicuna7b --------
if(!file.exists("./vicuna7b_data/results.csv")){
  # loop through all csv files under "./gpt4o_data" folder
  folder_path <- "./vicuna7b_data"
  csv_files <- list.files(path = folder_path, pattern = "answer_df_part[0-9]+\\.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_vicuna7b <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_vicuna7b, "./vicuna7b_data/results.csv", row.names = F)
}else{
  labeled_df_vicuna7b <- read.csv("./vicuna7b_data/results.csv", stringsAsFactors = F)
}


# ------ 7. mistral7b --------
if(!file.exists("./mistral_data/results.csv")){
  # loop through all csv files under "./gpt4o_data" folder
  folder_path <- "./mistral_data"
  csv_files <- list.files(path = folder_path, pattern = "answer_df_part[0-9]+\\.csv", full.names = TRUE)
  answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df_mistral <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df_mistral, "./mistral_data/results.csv", row.names = F)
}else{
  labeled_df_mistral <- read.csv("./mistral_data/results.csv", stringsAsFactors = F)
}



# ------ viz distribution ------

# additional info
text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
info_df <- text_df %>% select(sm_id, group, sr_name, url)
labeled_df_gpt4o <- merge(labeled_df_gpt4o, info_df, by = "sm_id", all.x = TRUE)
labeled_df_gpt4o[is.na(labeled_df_gpt4o)] <- 0
labeled_df_gpt4omini <- merge(labeled_df_gpt4omini, info_df, by = "sm_id", all.x = TRUE)
labeled_df_gpt4omini[is.na(labeled_df_gpt4omini)] <- 0
labeled_df_llama8b <- merge(labeled_df_llama8b, info_df, by = "sm_id", all.x = TRUE)
labeled_df_llama8b[is.na(labeled_df_llama8b)] <- 0
labeled_df_qwen <- merge(labeled_df_qwen, info_df, by = "sm_id", all.x = TRUE)
labeled_df_qwen[is.na(labeled_df_qwen)] <- 0
labeled_df_vicuna13b <- merge(labeled_df_vicuna13b, info_df, by = "sm_id", all.x = TRUE)
labeled_df_vicuna13b[is.na(labeled_df_vicuna13b)] <- 0
labeled_df_vicuna7b <- merge(labeled_df_vicuna7b, info_df, by = "sm_id", all.x = TRUE)
labeled_df_vicuna7b[is.na(labeled_df_vicuna7b)] <- 0
labeled_df_mistral <- merge(labeled_df_mistral, info_df, by = "sm_id", all.x = TRUE)
labeled_df_mistral[is.na(labeled_df_mistral)] <- 0
rm(text_df)


# sanity check
label_df_ls = list(labeled_df_gpt4o,
                   labeled_df_gpt4omini,
                   labeled_df_llama8b,
                   labeled_df_qwen,
                   labeled_df_vicuna13b,
                   labeled_df_vicuna7b,
                   labeled_df_mistral)
llm_name_ls = list("GPT4o",
                   "GPT4o-mini",
                   "Llama8b",
                   "Qwen7b",
                   "Vicuna13b",
                   "Vicuna7b",
                   "Mistral7b")
plot_distribution(label_df_ls, llm_name_ls)

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


