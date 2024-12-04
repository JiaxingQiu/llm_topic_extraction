setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
library(irrCAC)


rm_outlier <- function(weights = "unweighted",
                       type=c("ac1", "fleiss")[1]){
  rm_ls <- list()
  for(topic in fea_df$fea){
    rm_ls[[topic]] <- list()
    label_data <- data.frame("llama" = label_df_llama[[topic]],
                             "mistral" = label_df_mistral[[topic]],
                             "qwen" = label_df_qwen[[topic]],
                             "vicuna7b" = label_df_vicuna7b[[topic]])
    if(type=="fleiss"){
      label_ac1 <- irrCAC::fleiss.kappa.raw(label_data)$est
    }else{
      label_ac1 <- irrCAC::gwet.ac1.raw(label_data)$est
    }
    label_ac1_ci <- gsub("\\)","",gsub("\\(","",label_ac1$conf.int))
    label_ac1_ci_low <- as.numeric(unlist(strsplit(label_ac1_ci,","))[1])
    label_ac1_ci_up <- as.numeric(unlist(strsplit(label_ac1_ci,","))[2])
    
    ac1_rm1 <- function(data, coder_index) {
      modified_data <- data[, -coder_index, drop=FALSE]
      if(type=="fleiss"){
        ac1_sub <- irrCAC::fleiss.kappa.raw(modified_data)$est$coeff.val
      }else{
        ac1_sub <- irrCAC::gwet.ac1.raw(modified_data)$est$coeff.val
      }
      return(ac1_sub)  
    }
    ac1_results <- sapply(1:ncol(label_data), function(i) ac1_rm1(label_data, i))
    names(ac1_results) <- colnames(label_data)
    rm_ls[[topic]][["label"]] <- list("alpha_est" = round(label_ac1$coeff.val,3),
                                      "alpha_inc" = ac1_results,
                                      "alpha_inc_ratio" = ac1_results - label_ac1_ci_up)
    
    score_data <- data.frame("llama" = score_df_llama[[topic]],
                             "mistral" = score_df_mistral[[topic]],
                             "qwen" = score_df_qwen[[topic]],
                             "vicuna7b" = score_df_vicuna7b[[topic]])
    score_data <- data.frame(lapply(score_data, function(col) {
      cut(col, breaks = seq(0, 1, by = 0.1), labels = FALSE, include.lowest = TRUE)
    }))
    if(type=="fleiss"){
      score_ac1 <- irrCAC::fleiss.kappa.raw(score_data, weights = weights)$est
    }else{
      score_ac1 <- irrCAC::gwet.ac1.raw(score_data, weights = weights)$est
    }
    score_ac1_ci <- gsub("\\)","",gsub("\\(","",score_ac1$conf.int))
    score_ac1_ci_low <- as.numeric(unlist(strsplit(score_ac1_ci,","))[1])
    score_ac1_ci_up <- as.numeric(unlist(strsplit(score_ac1_ci,","))[2])
    ac1_results_score <- sapply(1:ncol(score_data), function(i) ac1_rm1(score_data, i))
    names(ac1_results_score) <- colnames(score_data)
    rm_ls[[topic]][["score"]] <- list("alpha_est" = round(score_ac1$coeff.val,3),
                                      "alpha_inc" = ac1_results_score,
                                      "alpha_inc_ratio" = ac1_results_score - score_ac1_ci_up)
    
  }
  return(rm_ls)
}


# ---- data 2 ----
source("./scripts/data_eng/prepare_analysis2.R")
rm_ls <- rm_outlier()
saveRDS(rm_ls, file = "./res/2/outliers.RDS")


# ---- data 1 ----
source("./scripts/data_eng/prepare_analysis.R")
rm_ls <- rm_outlier()
saveRDS(rm_ls, file = "./res/1/outliers.RDS")
