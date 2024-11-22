setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis.R")

# ---- ensemble score by PCA ----
score_df_pca <- readRDS("./data/score_df_pca.RDS")

# ---- ensemble agreement label df ----
agree_ratio = 0.1
label_df_ls <- get_label_agreed_by_dfs(list("llama" = label_df_llama, 
                                            "mini" = label_df_mini,
                                            "vicuna13b" = label_df_vicuna13b,
                                            "vicuna7b" = label_df_vicuna7b,
                                            "mistral" = label_df_mistral,
                                            "qwen" = label_df_qwen ), agree_ratio)
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
label_df_agreed_union <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)
agree_ratio = 0.5
label_df_ls <- get_label_agreed_by_dfs(list("llama" = label_df_llama, 
                                            "mini" = label_df_mini,
                                            "vicuna13b" = label_df_vicuna13b,
                                            "vicuna7b" = label_df_vicuna7b,
                                            "mistral" = label_df_mistral,
                                            "qwen" = label_df_qwen ), agree_ratio)
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
label_df_agreed_inter <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)

# find thresholds
t_df <- explore_thresholds(label_df_agreed_union,
                   score_df_pca,
                   fea_df,
                   label_df_agreed_inter)
pl <- list()
for(topic in fea_df$fea){
  plot_df <- t_df[which(t_df$topic==topic),]
  plot_df$f1_max = max(plot_df$f1_score, na.rm=T)
  plot_df$x_f1_max = plot_df$t[which(plot_df$f1_score==max(plot_df$f1_score))][1]
  pl[[topic]] <- ggplot(plot_df) +
    geom_point(aes(x = x_f1_max, y = f1_max), color='black') +
    geom_line(aes(x = t, y = f1_score),color='red') +
    geom_line(aes(x = t, y = sensitivity),color='orange') +
    geom_line(aes(x = t, y = precision),color='green3') +
    geom_line(aes(x = t, y = specificity),color='lightblue3') +
    labs(y=NULL, x=topic) + ylim(0.2, 1)
}
ggpubr::ggarrange(plotlist = pl, ncol=4, nrow = 5)


threshold_df <- t_df %>% group_by(topic) %>% summarise(threshold = t[which(f1_score==max(f1_score))][1])
threshold_df$fea <- threshold_df$topic
threshold_df <- as.data.frame(threshold_df)
threshold_df <- merge(threshold_df, fea_df, by = "fea")
# threshold_df$threshold <- threshold_df$threshold - 0.1
adjust_label_df <- adjust_label_by_threshold_score(label_df_agreed_union,
                                                   score_df_pca,
                                                   threshold_df)

# ----- evaluation ---- 
human_df <- read.csv("./human_data/sampled_posts.csv")
label_true <- label_df_gpt4o#[which(label_df_gpt4o$sm_id %in% human_df$sm_id),]

bal <- F
eval_pca <- eval_llm(score_df_pca, label_true, bal)
eval_pca$llm <- "pca_score"
# eval_agree_pca <- eval_llm(score_df_pca, label_true, bal, findoptimal = T)
# eval_agree_pca$llm <- "pca_label"
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
eval_agree_u <- eval_llm(label_df_agreed_union, label_true, bal)
eval_agree_u$llm <- "union_label"
eval_agree_i <- eval_llm(label_df_agreed_inter, label_true, bal)
eval_agree_i$llm <- "inter_label"
eval_agree_adj <- eval_llm(adjust_label_df, label_true, bal)
eval_agree_adj$llm <- "pca_label"


eval_df <- rbind(eval_agree_adj, eval_agree_u, eval_agree_i,
                 # eval_agree_pca, 
                 eval_pca, 
                 eval_mini, eval_mini_s, 
                 eval_llama, eval_llama_s,
                 eval_qwen, eval_qwen_s,
                 eval_vicuna13b, eval_vicuna13b_s,
                 eval_vicuna7b, eval_vicuna7b_s,
                 eval_mistral, eval_mistral_s) 
eval_df$llm <- factor(eval_df$llm, 
                      levels = c("pca_score", "pca_label", 
                                 "pca_adjust_label", 
                                 "gpt4omini_score", "gpt4omini_label",
                                 "llama_score", "llama_label",
                                 "qwen_score", "qwen_label",
                                 "mistral_score", "mistral_label",
                                 "vicuna13b_score","vicuna13b_label",
                                 "vicuna7b_score","vicuna7b_label")) #, "union_label", "inter_label"
# color_scale <- c("pca_adjust_label" = "red","pca_score" = "red",
#                  "union_label" = "grey",
#                  "inter_label" = "darkgrey",
#                  "gpt4omini_label" = "green3","gpt4omini_score" = "green3",
#                  "llama_label" = "steelblue","llama_score" = "steelblue",
#                  "qwen_label" = "orange","qwen_score" = "orange",
#                  "mistral_label" = "pink2","mistral_score" = "pink2",
#                  "vicuna13b_label" = "pink2","vicuna13b_score" = "pink2",
#                  "vicuna7b_label" = "pink2","vicuna7b_score" = "pink2")
# Plot using ggplot2 with rotated x-axis labels
p1 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm), "_label")),], aes(x = as.factor(topic), y = f1_score, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + #scale_fill_manual(values=color_scale) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 8) )+ labs(x=NULL)
p2 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm), "_label")),], aes(x = as.factor(topic), y = sensitivity, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + #scale_fill_manual(values=color_scale) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 8) )+ labs(x=NULL)
p4 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm), "_label")),], aes(x = as.factor(topic), y = specificity, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + #scale_fill_manual(values=color_scale) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 8) )+ labs(x=NULL)
p3 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm), "_label")),], aes(x = as.factor(topic), y = precision, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + #scale_fill_manual(values=color_scale) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 8) ) + labs(x=NULL)

# ggarrange(p1,p2,p3, ncol=1, common.legend = T, legend = "right")
p5 <- ggplot(eval_df[which(endsWith(as.character(eval_df$llm), "_score")),], aes(x = as.factor(topic), y = auprc, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + #scale_fill_manual(values=color_scale) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 8) ) + labs(x=NULL)

ggarrange(ggarrange(p1,p2,p3, ncol=1, common.legend = T, legend = "right"),
          p5,
          ncol=1, common.legend = F,
          heights = c(3,1))


# pl <- list()
# for(topic in fea_df$fea){
#   pl[[topic]] <- cali_plot(score_df_pca[[topic]], 
#                            label_df_agreed[[topic]])[["p_cali"]] +
#     ggtitle(topic) +labs(x=NULL, y=NULL)+xlim(0,0.5)
# }
# f <- ggpubr::ggarrange(plotlist = pl, ncol=4, nrow = 5)
# annotate_figure(
#   f,
#   bottom = text_grob("Threshold on ensemble cosine score", size = 14),
#   left = text_grob("Positive rate of ensemble label < a threshold (rescaled)", size = 14, rot = 90)
# )

