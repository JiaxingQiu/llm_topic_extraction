setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis.R")
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
human_df <- read.csv("./human_data/sampled_posts.csv")
label_true <- label_df_gpt4o[which(label_df_gpt4o$sm_id %in% human_df$sm_id),]
bal <- F

llm_full_list <- list("llama" = list("label_df" = label_df_llama,
                                    "score_df" = score_df_llama),
                      "mini" = list("label_df" = label_df_mini,
                                    "score_df" = score_df_mini),
                      "qwen" = list("label_df" = label_df_qwen,
                                    "score_df" = score_df_qwen),
                      "mistral" = list("label_df" = label_df_mistral,
                                       "score_df" = score_df_mistral),
                      "vicuna7b" = list("label_df" = label_df_vicuna7b,
                                        "score_df" = score_df_vicuna7b),
                      "vicuna13b" = list("label_df" = label_df_vicuna13b,
                                         "score_df" = score_df_vicuna13b))
# create all combination of c(1,2,3,4,5,6) that has at least 2 elements
vec <- c(1, 2, 3, 4, 5, 6)
combinations_at_least_two <- lapply(2:length(vec), function(k) combn(vec, k, simplify = FALSE))
combinations_at_least_two <- unlist(combinations_at_least_two, recursive = FALSE)
eval_pca <- list()
eval_agree_u <- list()
eval_agree_i <- list()
eval_agree_adj <- list()
pb <- txtProgressBar(min = 0, max = length(combinations_at_least_two), style = 3)
for(k in 1:length(combinations_at_least_two)) {
  cb <- combinations_at_least_two[[k]]
  llm_list <- llm_full_list[cb]
  # ---- ensemble score by PCA ----
  score_df_pca <- score_df_gpt4o # initiation
  for(topic in fea_df$fea){
    print(topic)
    pca_df <- NULL
    for(llm_name in names(llm_list) ){
      llm_dfs <- llm_list[[llm_name]]
      pca_df_llm <- data.frame("score" = llm_dfs$score_df[,topic],
                               "label" = llm_dfs$label_df[,topic])
      colnames(pca_df_llm) <- paste0(colnames(pca_df_llm), "_",llm_name)
      if(is.null(pca_df)) {pca_df <- pca_df_llm}
      else{pca_df <- bind_cols(pca_df, pca_df_llm)}
    }
    
    # PCA on this dataframe, with dimention = 2, keep the first demention
    pca_result <- prcomp(pca_df[,startsWith(colnames(pca_df), "score_")], center = TRUE, scale. = TRUE) 
    # Add the first dimension as a new column in the data frame
    pca_df$pca1 <- pca_result$x[, 1]
    # correction for the sign
    if(cor(pca_df$pca1, score_df_gpt4o[,topic])<0){
      pca_df$pca1 <- -pca_df$pca1
    }
    score_df_pca[,topic] <- (pca_df$pca1-min(pca_df$pca1)) / (max(pca_df$pca1) - min(pca_df$pca1))
  }
  # ---- ensemble agreement label df ----
  # get inter and union
  label_df_list <- list()
  for(llm_name in names(llm_list)){
    label_df_list[[llm_name]] <- llm_list[[llm_name]]$label_df
  }
  agree_ratio = 0.1
  label_df_ls <- get_label_agreed_by_dfs(label_df_list, agree_ratio)
  label_df_agreed_union <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)
  agree_ratio = 0.5
  label_df_ls <- get_label_agreed_by_dfs(label_df_list, agree_ratio)
  label_df_agreed_inter <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)
  rm(label_df_list, label_df_ls)
  
  # find thresholds
  t_df <- explore_thresholds(label_df_agreed_union,
                             score_df_pca,
                             fea_df,
                             label_df_agreed_inter)
  
  # adjust union label based on threshold
  threshold_df <- t_df %>% group_by(topic) %>% summarise(threshold = t[which(f1_score==max(f1_score))][1])
  threshold_df$fea <- threshold_df$topic
  threshold_df <- as.data.frame(threshold_df)
  threshold_df <- merge(threshold_df, fea_df, by = "fea")
  adjust_label_df <- adjust_label_by_threshold_score(label_df_agreed_union,
                                                     score_df_pca,
                                                     threshold_df)
  # ---- evaluation ---- 
  eval_pca[[k]] <- eval_llm(score_df_pca, label_true, bal)
  eval_pca[[k]]$llm <- paste0("pca_score (",length(cb)," llms)")
  eval_agree_u[[k]] <- eval_llm(label_df_agreed_union, label_true, bal)
  eval_agree_u[[k]]$llm <- paste0("union_label (",length(cb)," llms)")
  eval_agree_i[[k]] <- eval_llm(label_df_agreed_inter, label_true, bal)
  eval_agree_i[[k]]$llm <- paste0("inter_label (",length(cb)," llms)")
  eval_agree_adj[[k]] <- eval_llm(adjust_label_df, label_true, bal)
  eval_agree_adj[[k]]$llm <- paste0("pca_label (",length(cb)," llms)")
  # ---- Update progress bar ----
  setTxtProgressBar(pb, k)
}

# ----- individual llm's evaluation ---- 
eval_mini <- eval_llm(label_df_mini, label_true, bal)
eval_mini$llm <- "gpt4omini_label"
eval_mini_s <- eval_llm(score_df_mini, label_true, bal)
eval_mini_s$llm <- "gpt4omini_score"
eval_llama <- eval_llm(label_df_llama, label_true, bal)
eval_llama$llm <- "llama_label"
eval_llama_s <- eval_llm(score_df_llama, label_true, bal)
eval_llama_s$llm <- "llama_score"
eval_qwen <- eval_llm(label_df_qwen, label_true, bal)
eval_qwen$llm <- "qwen_label"
eval_qwen_s <- eval_llm(score_df_qwen, label_true, bal)
eval_qwen_s$llm <- "qwen_score"
eval_vicuna13b <- eval_llm(label_df_vicuna13b, label_true, bal)
eval_vicuna13b$llm <- "vicuna13b_label"
eval_vicuna13b_s <- eval_llm(score_df_vicuna13b, label_true, bal)
eval_vicuna13b_s$llm <- "vicuna13b_score"
eval_vicuna7b <- eval_llm(label_df_vicuna7b, label_df_gpt4o, bal)
eval_vicuna7b$llm <- "vicuna7b_label"
eval_vicuna7b_s <- eval_llm(score_df_vicuna7b, label_df_gpt4o, bal)
eval_vicuna7b_s$llm <- "vicuna7b_score"
eval_mistral <- eval_llm(label_df_mistral, label_df_gpt4o, bal)
eval_mistral$llm <- "mistral_label"
eval_mistral_s <- eval_llm(score_df_mistral, label_df_gpt4o, bal)
eval_mistral_s$llm <- "mistral_score"

