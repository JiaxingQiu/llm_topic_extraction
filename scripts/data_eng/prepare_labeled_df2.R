setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

# example: 104a474

rm(list=ls())

library(dplyr)
library(parallel)


path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
fea_df <- read.csv("./data2/fea_df.csv")


# ------ 1. llama 8b -------
if(!file.exists("./data2/results_llama.csv")){
  answer_df <- read.csv("./data2/answer_df_llama.csv", stringsAsFactors = F)
  answer_df$pat_id <- answer_df$sm_id
  answer_df$sm_id <- 1:nrow(answer_df)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df, "./data2/results_llama.csv", row.names = F)
}else{
  labeled_df_llama8b <- read.csv("./data2/results_llama.csv", stringsAsFactors = F)
}


# ------ 2. Qwen --------
if(!file.exists("./data2/results_qwen.csv")){
  answer_df <- read.csv("./data2/answer_df_qwen.csv", stringsAsFactors = F)
  answer_df$pat_id <- answer_df$sm_id
  answer_df$sm_id <- 1:nrow(answer_df)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df, "./data2/results_qwen.csv", row.names = F)
}else{
  labeled_df_qwen <- read.csv("./data2/results_qwen.csv", stringsAsFactors = F)
}

# ------ 3. Vicuna7b --------
if(!file.exists("./data2/results_vicuna.csv")){
  answer_df <- read.csv("./data2/answer_df_vicuna.csv", stringsAsFactors = F)
  answer_df$pat_id <- answer_df$sm_id
  answer_df$sm_id <- 1:nrow(answer_df)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df, "./data2/results_vicuna.csv", row.names = F)
}else{
  labeled_df_vicuna7b <- read.csv("./data2/results_vicuna.csv", stringsAsFactors = F)
}

# ------ 4. mistral7b --------
if(!file.exists("./data2/results_mistral.csv")){
  answer_df <- read.csv("./data2/answer_df_mistral.csv", stringsAsFactors = F)
  answer_df$pat_id <- answer_df$sm_id
  answer_df$sm_id <- 1:nrow(answer_df)
  answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
  labeled_df <- get_result_df(answer_df, fea_df$fea)
  write.csv(labeled_df, "./data2/results_mistral.csv", row.names = F)
}else{
  labeled_df_mistral <- read.csv("./data2/results_mistral.csv", stringsAsFactors = F)
}


# ------ viz distribution ------
labeled_df_llama8b[is.na(labeled_df_llama8b)] <- 0
labeled_df_llama8b$stigma <- ifelse(rowSums(labeled_df_llama8b[,fea_df$fea])>0,1,0)
labeled_df_qwen[is.na(labeled_df_qwen)] <- 0
labeled_df_qwen$stigma <- ifelse(rowSums(labeled_df_qwen[,fea_df$fea])>0,1,0)
labeled_df_vicuna7b[is.na(labeled_df_vicuna7b)] <- 0
labeled_df_vicuna7b$stigma <- ifelse(rowSums(labeled_df_vicuna7b[,fea_df$fea])>0,1,0)
labeled_df_mistral[is.na(labeled_df_mistral)] <- 0
labeled_df_mistral$stigma <- ifelse(rowSums(labeled_df_mistral[,fea_df$fea])>0,1,0)

topic_distribution_df <- data.frame()
for(topic in c("stigma",fea_df$fea) ){
  topic_sum <- data.frame(topic = topic,
                          llama = mean(labeled_df_llama8b[,topic]), 
                          qwen = mean(labeled_df_qwen[,topic]),
                          vicuna = mean(labeled_df_vicuna7b[,topic]),
                          mistral = mean(labeled_df_mistral[,topic]) )
  topic_distribution_df <- bind_rows(topic_distribution_df, topic_sum)
}
topic_distribution_df

