setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

library(dplyr)
library(parallel)
library(ggplot2)
library(ggpubr)

path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

# prepare label_df, score_df
fea_df <- read.csv("./data/fea_df.csv")

# small models and agreements
label_df_ls <- get_label_agreed(llm_ls = c("gpt_data", "llama_data"))
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
label_df_agreed <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)
labeled_df_gpt4omini <- merge(label_df_ls$gpt_data, info_df, by = "sm_id", all.x = TRUE)
labeled_df_llama8b <- merge(label_df_ls$llama_data, info_df, by = "sm_id", all.x = TRUE)
# large model as reference
labeled_df_gpt4o <- read.csv("./gpt4o_data/results.csv", stringsAsFactors = F)
labeled_df_gpt4o <- merge(labeled_df_gpt4o, info_df, by = "sm_id", all.x = TRUE)
# score dataframes
obj <- get_score_df("gpt_data", "stella_en_1.5B_v5")
score_df_gpt4omini <- obj$scores_df
obj <- get_score_df("llama_data", "stella_en_1.5B_v5")
score_df_llama8b <- obj$scores_df



# adjust label using score and multiple llm label agreement
adjust_labeled_df_llama8b <- adjust_label_by_agreement(
  label_df = labeled_df_llama8b,
  score_df = score_df_llama8b,
  true_label_df = label_df_agreed)
adjust_labeled_df_gpt4omini <- adjust_label_by_agreement(
  label_df = labeled_df_gpt4omini,
  score_df = score_df_gpt4omini,
  true_label_df = label_df_agreed)
# get new agreement
adjust_label_df_ls <- get_label_agreed_by_dfs(list("llama8b_adj" = adjust_labeled_df_llama8b, 
                                                     "gpt4omini_adj" = adjust_labeled_df_gpt4omini),
                                              agree_ratio = 0.1)

adjust_label_df_agreed <- merge(adjust_label_df_ls$agreed[,c("sm_id", fea_df$fea)], info_df, by = "sm_id", all.x = TRUE)
adjust_labeled_df_gpt4omini <- merge(adjust_label_df_ls$gpt4omini_adj[,c("sm_id", fea_df$fea)], info_df, by = "sm_id", all.x = TRUE)
adjust_labeled_df_llama8b <- merge(adjust_label_df_ls$llama8b_adj[,c("sm_id", fea_df$fea)], info_df, by = "sm_id", all.x = TRUE)

# get evaluation scores
eval_agree <- eval_llm(label_df_agreed, labeled_df_gpt4o)
eval_agree$llm <- "agreement"
eval_adjust_agree <- eval_llm(adjust_label_df_agreed, labeled_df_gpt4o)
eval_adjust_agree$llm <- "adjust_agreement"

eval_gpt4omini <- eval_llm(labeled_df_gpt4omini, labeled_df_gpt4o)
eval_gpt4omini$llm <- "gpt4omini"
eval_gpt4omini_score <- eval_llm(score_df_gpt4omini, labeled_df_gpt4o)
eval_gpt4omini_score$llm <- "gpt4omini_score"
eval_gpt4omini_adj <- eval_llm(adjust_labeled_df_gpt4omini, labeled_df_gpt4o)
eval_gpt4omini_adj$llm <- "gpt4omini_adjust"

eval_llama8b <- eval_llm(labeled_df_llama8b, labeled_df_gpt4o)
eval_llama8b$llm <- "llama8b"
eval_llama8b_score <- eval_llm(score_df_llama8b, labeled_df_gpt4o)
eval_llama8b_score$llm <- "llama8b_score"
eval_llama8b_adj <- eval_llm(adjust_labeled_df_llama8b, labeled_df_gpt4o)
eval_llama8b_adj$llm <- "llama8b_adjust"

eval_df <- rbind(eval_agree, #eval_adjust_agree,
                 eval_gpt4omini, eval_gpt4omini_score, #eval_gpt4omini_adj,
                 eval_llama8b, eval_llama8b_score ) #eval_llama8b_adj
# Plot using ggplot2 with rotated x-axis labels
p1 <- ggplot(eval_df, aes(x = as.factor(topic), y = f1_score, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p2<- ggplot(eval_df, aes(x = as.factor(topic), y = auprc, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggarrange(p1,p2, ncol=1)




# for test
eval_raw <- eval_llm(labeled_df_llama8b, label_df_agreed)
eval_adj <- eval_llm(adjust_labeled_df_llama8b, label_df_agreed)
cbind(eval_raw[,c("auprc","f1_score")],eval_adj[,c("auprc","f1_score")])
eval_raw <- eval_llm(labeled_df_gpt4omini, label_df_agreed)
eval_adj <- eval_llm(adjust_labeled_df_gpt4omini, label_df_agreed)
cbind(eval_raw[,c("auprc","f1_score")],eval_adj[,c("auprc","f1_score")])


