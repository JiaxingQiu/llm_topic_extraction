setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
# install.packages("irr")
library(irr)

# ---- data 2 ----
source("./scripts/data_eng/prepare_analysis2.R")
rm_ls <- list()
for(topic in c("stigma")){
  data <- data.frame("llama" = label_df_llama[[topic]],
                     "mistral" = label_df_mistral[[topic]],
                     "qwen" = label_df_qwen[[topic]],
                     "vicuna" = label_df_vicuna7b[[topic]])
  data <- as.matrix(data)
  # alpha using all coders
  original_alpha <- kripp.alpha(data, method="nominal")$value
  # Function to calculate and compare alpha excluding one coder
  alpha_increase <- function(data, coder_index) {
    modified_data <- data[, -coder_index, drop=FALSE]
    new_alpha <- kripp.alpha(modified_data, method="nominal")$value
    return(original_alpha - new_alpha)  # True if exclusion increases alpha beyond threshold
  }
  alpha_diff_results <- sapply(1:ncol(data), function(i) alpha_increase(data, i))
  names(alpha_diff_results) <- colnames(data)
  # if the increase is larger than a threashold, the coder is an outlier
  rm_ls[[topic]] <- c(names(alpha_diff_results)[which(alpha_diff_results>0.01)]) # great qwen's value is the largest
}
saveRDS(rm_ls, file = "./res/2/outliers.RDS")


# ---- data 1 ----
source("./scripts/data_eng/prepare_analysis.R")
human_df <- read.csv("./human_data/sampled_posts.csv")
rm_ls <- list()
for(topic in fea_df$fea){
  print(topic)
  data <- data.frame("mini" = label_df_mini[which(label_df_mini$sm_id %in% human_df$sm_id),topic],
                     "llama" = label_df_llama[which(label_df_llama$sm_id %in% human_df$sm_id),topic],
                     "mistral" = label_df_mistral[which(label_df_mistral$sm_id %in% human_df$sm_id),topic],
                     "qwen" = label_df_qwen[which(label_df_qwen$sm_id %in% human_df$sm_id),topic],
                     "vicuna" = label_df_vicuna7b[which(label_df_vicuna7b$sm_id %in% human_df$sm_id),topic])
  data <- as.matrix(data)
  # alpha using all coders
  original_alpha <- kripp.alpha(data, method="nominal")$value
  print(original_alpha)
  # Function to calculate and compare alpha excluding one coder
  alpha_increase <- function(data, coder_index) {
    modified_data <- data[, -coder_index, drop=FALSE]
    new_alpha <- kripp.alpha(modified_data, method="nominal")$value
    return(original_alpha - new_alpha)  # True if exclusion increases alpha beyond threshold
  }
  alpha_diff_results <- sapply(1:ncol(data), function(i) alpha_increase(data, i))
  names(alpha_diff_results) <- colnames(data)
  print(alpha_diff_results)
  outliers <- c(names(alpha_diff_results)[which(alpha_diff_results>0.01)]) # great qwen's value is the largest
  print(outliers)
  # if the increase is larger than a threashold, the coder is an outlier
  rm_ls[[topic]] <- outliers
  saveRDS(rm_ls, file = "./res/1/outliers.RDS")
}
saveRDS(rm_ls, file = "./res/1/outliers.RDS")

