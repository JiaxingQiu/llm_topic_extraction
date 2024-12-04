setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
library(irrCAC)
weights <- "unweighted"

# ---- data 2 ----
source("./scripts/data_eng/prepare_analysis2.R")
rm_ls <- list()
for(topic in c("stigma")){
  data <- data.frame(
    "llama" = score_df_llama[,topic],
    "mistral" = score_df_mistral[,topic],
    "qwen" = score_df_qwen[,topic],
    "vicuna7b" = score_df_vicuna7b[,topic])
  
  data <- data.frame(lapply(data, function(col) {
    col <- factor(cut(col, breaks = seq(0, 1, by = 0.2), labels = FALSE, include.lowest = TRUE), levels = c(1,2,3,4,5))
    levels(col) <- seq(0, 1, by = 0.2)[2:6]
    col
  }))
  ac2_result <- irrCAC::gwet.ac1.raw(data, weights = weights)
  ac2_est <- ac2_result$est$coeff.val # #Yields AC1 coefficient with precision measures
  ac2_ci <- gsub("\\)","",gsub("\\(","",ac2_result$est$conf.int))
  ac2_ci_low <- as.numeric(unlist(strsplit(ac2_ci,","))[1])
  ac2_ci_up <- as.numeric(unlist(strsplit(ac2_ci,","))[2])
  
  # Function to calculate and compare alpha excluding one coder
  ac2_rm1 <- function(data, coder_index) {
    modified_data <- data[, -coder_index, drop=FALSE]
    # ac2_sub_ci <- irrCAC::gwet.ac1.raw(modified_data, weights = weights)$est$conf.int
    # ac2_sub_ci <- gsub("\\)","",gsub("\\(","",ac2_sub_ci))
    # ac2_sub <- as.numeric(unlist(strsplit(ac2_sub_ci,","))[1])
    ac2_sub <- irrCAC::gwet.ac1.raw(modified_data, weights = weights)$est$coeff.val
    return(ac2_sub)  # True if exclusion increases alpha beyond threshold
  }
  ac2_results <- sapply(1:ncol(data), function(i) ac2_rm1(data, i))
  names(ac2_results) <- colnames(data)
  rm_ls[[topic]] <- list("ac2_format" = paste0(round(ac2_est,3)," ",ac2_result$est$conf.int),
                         "alpha_all" = ac2_est,
                         "alpha_inc" = ac2_results,
                         "alpha_inc_ratio" = ac2_results - ac2_ci_up)
}
print(rm_ls)
saveRDS(rm_ls, file = "./res/2/outliers_AC2.RDS")


# ---- data 1 ----
source("./scripts/data_eng/prepare_analysis.R")
human_df <- read.csv("./human_data/sampled_posts.csv")
rm_ls <- list()
for(topic in fea_df$fea){
  data <- data.frame(
    "llama" = score_df_llama[,topic],
    "mistral" = score_df_mistral[,topic],
    "qwen" = score_df_qwen[,topic],
    "vicuna7b" = score_df_vicuna7b[,topic])
  
  data <- data.frame(lapply(data, function(col) {
    col <- factor(cut(col, breaks = seq(0, 1, by = 0.2), labels = FALSE, include.lowest = TRUE), levels = c(1,2,3,4,5))
    levels(col) <- seq(0, 1, by = 0.2)[2:6]
    col
  }))
  ac2_result <- irrCAC::gwet.ac1.raw(data, weights = weights)
  ac2_est <- ac2_result$est$coeff.val # #Yields AC1 coefficient with precision measures
  ac2_ci <- gsub("\\)","",gsub("\\(","",ac2_result$est$conf.int))
  ac2_ci_low <- as.numeric(unlist(strsplit(ac2_ci,","))[1])
  ac2_ci_up <- as.numeric(unlist(strsplit(ac2_ci,","))[2])
  
  # Function to calculate and compare alpha excluding one coder
  ac2_rm1 <- function(data, coder_index) {
    modified_data <- data[, -coder_index, drop=FALSE]
    # ac2_sub_ci <- irrCAC::gwet.ac1.raw(modified_data, weights = weights)$est$conf.int
    # ac2_sub_ci <- gsub("\\)","",gsub("\\(","",ac2_sub_ci))
    # ac2_sub <- as.numeric(unlist(strsplit(ac2_sub_ci,","))[1])
    ac2_sub <- irrCAC::gwet.ac1.raw(modified_data, weights = weights)$est$coeff.val
    return(ac2_sub)  # True if exclusion increases alpha beyond threshold
  }
  ac2_results <- sapply(1:ncol(data), function(i) ac2_rm1(data, i))
  names(ac2_results) <- colnames(data)
  rm_ls[[topic]] <- list("ac2_format" = paste0(round(ac2_est,3)," ",ac2_result$est$conf.int),
                         "alpha_all" = ac2_est,
                         "alpha_inc" = ac2_results,
                         "alpha_inc_ratio" = ac2_results - ac2_ci_up)
}
print(rm_ls)
saveRDS(rm_ls, file = "./res/1/outliers_AC2.RDS")
