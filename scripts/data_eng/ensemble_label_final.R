setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis.R")
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
label_true <- read.csv("./human_data/results.csv")
bal <- F
rm_ls <- readRDS("./res/1/outliers.RDS")

llm_full_list <- list("llama" = list("label_df" = label_df_llama,
                                     "score_df" = score_df_llama),
                      "qwen" = list("label_df" = label_df_qwen,
                                    "score_df" = score_df_qwen),
                      "mistral" = list("label_df" = label_df_mistral,
                                       "score_df" = score_df_mistral),
                      "vicuna7b" = list("label_df" = label_df_vicuna7b,
                                        "score_df" = score_df_vicuna7b)
)

agree_ratio_inter <- 0.5 # at least half
outlier_cut <- 0.1 # percentage of increase
res_filename <- paste0("./res/1/ensemble_dfs.RData") 


library(dplyr)
ensemble_label_df <- label_df_llama
ensemble_score_df <- score_df_llama

fea_df_full <- fea_df
for(topic in fea_df_full$fea){
  fea_df <- fea_df_full[which(fea_df_full$fea==topic),]
  os <- (rm_ls[[topic]][["label"]]$alpha_inc_ratio + rm_ls[[topic]][["score"]]$alpha_inc_ratio)/2
  est_avg <- (rm_ls[[topic]][["label"]]$alpha_est + rm_ls[[topic]][["score"]]$alpha_est)/2
  os <- os/est_avg # percentage of increase
  opt_cb <- setdiff(names(llm_full_list), c(names(os)[which(os>outlier_cut)])) # remove outlier
  if(topic == "protein"){
    opt_cb <- names(llm_full_list)
  }
  opt_res <- ensemble_sample(opt_cb, return_label=T)
  ensemble_label_df[,topic] <- opt_res$ensemble_label[,topic]
  ensemble_score_df[,topic] <- opt_res$ensemble_score[,topic]
}
ensemble_label_df <- ensemble_label_df[,c("sm_id", fea_df_full$fea)]
ensemble_score_df <- ensemble_score_df[,c("sm_id", fea_df_full$fea)]
ens_obj <- list("label" = ensemble_label_df, 
                "score" = ensemble_score_df)
save(ens_obj, file=res_filename)
