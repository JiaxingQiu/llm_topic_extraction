setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis.R")


# ---- ensemble score by PCA ----
score_df_pca <- score_df_gpt4o
for(topic in fea_df$fea){
  print(topic)
  pca_df <- data.frame(llm1_score = score_df_llama[,topic],
                   llm1_label = label_df_llama[,topic],
                   llm2_score = score_df_mini[,topic],
                   llm2_label = label_df_mini[,topic],
                   llm3_score = score_df_qwen[,topic],
                   llm3_label = label_df_qwen[,topic],
                   llm4_score = score_df_vicuna13b[,topic],
                   llm4_label = label_df_vicuna13b[,topic],
                   llm5_score = score_df_vicuna7b[,topic],
                   llm5_label = label_df_vicuna7b[,topic],
                   llm6_score = score_df_mistral[,topic],
                   llm6_label = label_df_mistral[,topic])
  pca_df$llm_count <- pca_df$llm1_label + pca_df$llm2_label + pca_df$llm3_label + pca_df$llm4_label + pca_df$llm5_label + pca_df$llm6_label# 
  # PCA on this dataframe, with dimention = 2, keep the first demention
  pca_result <- prcomp(pca_df[,c("llm1_score", "llm2_score", "llm3_score", "llm4_score", "llm5_score", "llm6_score")], center = TRUE, scale. = TRUE) # , "llm_count"
  # Add the first dimension as a new column in the data frame
  pca_df$pca1 <- pca_result$x[, 1]
  
  # plot(pca_df$pca1, score_df_gpt4o[,topic])
  # hist(pca_df$pca1)
  # hist(score_df_llama[,topic])
  # hist(score_df_mini[,topic])
  
  # correction for the sign
  if(cor(pca_df$pca1, score_df_mini[,topic])<0){
    pca_df$pca1 <- -pca_df$pca1
  }
  score_df_pca[,topic] <- (pca_df$pca1-min(pca_df$pca1)) / (max(pca_df$pca1) - min(pca_df$pca1))
}
colnames(score_df_pca)
saveRDS(score_df_pca, file = "./data/score_df_pca.RDS")

# # ---- ensemble agreement label df ----
# label_df_ls <- get_label_agreed_by_dfs(list("llama" = label_df_llama, 
#                                             "mini" = label_df_mini,
#                                             "qwen" = label_df_qwen), 0.5)
# info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
# label_df_agreed <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)
# 
# adjust_labeled_df_agree <- adjust_label_by_agreement(
#   label_df = label_df_agreed,
#   score_df = score_df_pca,
#   true_label_df = label_df_agreed,
#   plot_topic = c("feargain", "thinspo"))
# 
# # ----- evaluation ---- 
bal <- F
eval_pred <- eval_llm(score_df_pca, label_df_gpt4o, bal)
eval_pred$llm <- "pca_ensemble_score"
eval_mini <- eval_llm(label_df_mini, label_df_gpt4o, bal)
eval_mini$llm <- "gpt4omini_label"
eval_mini_s <- eval_llm(score_df_mini, label_df_gpt4o, bal)
eval_mini_s$llm <- "gpt4omini_score"
eval_llama <- eval_llm(label_df_llama, label_df_gpt4o, bal)
eval_llama$llm <- "llama_label"
eval_llama_s <- eval_llm(score_df_llama, label_df_gpt4o, bal)
eval_llama_s$llm <- "llama_score"
eval_qwen <- eval_llm(label_df_qwen, label_df_gpt4o, bal)
eval_qwen$llm <- "qwen_label"
eval_qwen_s <- eval_llm(score_df_qwen, label_df_gpt4o, bal)
eval_qwen_s$llm <- "qwen_score"
eval_vicuna13b <- eval_llm(label_df_vicuna13b, label_df_gpt4o, bal)
eval_vicuna13b$llm <- "vicuna13b_label"
eval_vicuna13b_s <- eval_llm(score_df_vicuna13b, label_df_gpt4o, bal)
eval_vicuna13b_s$llm <- "vicuna13b_score"
eval_vicuna7b <- eval_llm(label_df_vicuna7b, label_df_gpt4o, bal)
eval_vicuna7b$llm <- "vicuna7b_label"
eval_vicuna7b_s <- eval_llm(score_df_vicuna7b, label_df_gpt4o, bal)
eval_vicuna7b_s$llm <- "vicuna7b_score"
eval_mistral <- eval_llm(label_df_mistral, label_df_gpt4o, bal)
eval_mistral$llm <- "mistral_label"
eval_mistral_s <- eval_llm(score_df_mistral, label_df_gpt4o, bal)
eval_mistral_s$llm <- "mistral_score"


eval_df <- rbind(eval_pred, 
                 eval_mini, eval_mini_s, 
                 eval_llama, eval_llama_s,
                 eval_qwen, eval_qwen_s,
                 eval_vicuna13b, eval_vicuna13b_s,
                 eval_vicuna7b, eval_vicuna7b_s,
                 eval_mistral, eval_mistral_s) 
eval_df$llm <- factor(eval_df$llm, 
                      levels = c("pca_ensemble_score", "agreed_label", 
                                 "gpt4omini_score", "gpt4omini_label",
                                 "llama_score", "llama_label",
                                 "qwen_score", "qwen_label",
                                 "mistral_score", "mistral_label",
                                 "vicuna13b_score", "vicuna13b_label",
                                 "vicuna7b_score", "vicuna7b_label"))

# Plot using ggplot2 with rotated x-axis labels
p1 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm),"_label")),], aes(x = as.factor(topic), y = f1_score, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p2 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm),"_score")),], aes(x = as.factor(topic), y = auprc, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggarrange(p1,p2, ncol=1)

# --- cali plot ---
# score_df_plot <- score_df_vicuna13b
# label_df_plot <- label_df_vicuna13b
# score_df_plot <- score_df_qwen
# label_df_plot <- label_df_qwen
# score_df_plot <- score_df_llama
# label_df_plot <- label_df_llama
# score_df_plot <- score_df_mini
# label_df_plot <- label_df_mini

# pl <- list()
# for(topic in fea_df$fea){
#   pl[[topic]] <- cali_plot(score_df_plot[[topic]],
#                            label_df_plot[[topic]])[["p_cali"]] +
#     ggtitle(topic) +labs(x=NULL, y=NULL)
# }
# f <- ggpubr::ggarrange(plotlist = pl, ncol=4, nrow = 5)
# annotate_figure(
#   f,
#   bottom = text_grob("Threshold on ensemble cosine score", size = 14),
#   left = text_grob("Positive rate of ensemble label < a threshold (rescaled)", size = 14, rot = 90)
# )
