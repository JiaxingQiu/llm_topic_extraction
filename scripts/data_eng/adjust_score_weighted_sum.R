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
obj <- get_score_df("gpt_data", "stella_en_1.5B_v5")
score_df_mini <- obj$scores_df
label_df_mini <- obj$labeled_df
obj <- get_score_df("llama_data", "stella_en_1.5B_v5")
score_df_llama <- obj$scores_df
label_df_llama <- obj$labeled_df
obj <- get_score_df("gpt4o_data", "stella_en_1.5B_v5")
score_df_gpt4o <- obj$scores_df
label_df_gpt4o <- obj$labeled_df

# aggregate score by weighted sum
# weights <- c(0.3,0.7)
# by weights[1] * score_df_llama[,topic] + weights[2] * score_df_mini[,topic]
score_df_sum <- score_df_gpt4o
for(topic in fea_df$fea){
  print(topic)
  df <- data.frame(llm1_score = score_df_llama[,topic],
                       llm1_label = label_df_llama[,topic],
                       llm2_score = score_df_mini[,topic],
                       llm2_label = label_df_mini[,topic])
  
  df_cali <- cali_plot(df$llm1_score, df$llm1_label)
  df_cali$diff <- df_cali$y_cali_observed - df_cali$y_cali_predicted
  df_cali$diff[df_cali$diff<0] <- 0
  aucc1 <- sum(df_cali$diff)# get AU calibration curve
  
  df_cali <- cali_plot(df$llm2_score, df$llm2_label,)
  df_cali$diff <- df_cali$y_cali_observed - df_cali$y_cali_predicted
  df_cali$diff[df_cali$diff<0] <- 0
  aucc2 <- sum(df_cali$diff)# get AU calibration curve
  
  df_cali <- cali_plot(score_df_gpt4o[,topic], label_df_gpt4o[,topic])
  df_cali$diff <- df_cali$y_cali_observed - df_cali$y_cali_predicted
  df_cali$diff[df_cali$diff<0] <- 0
  aucc3 <- sum(df_cali$diff)# get AU calibration curve
  
  print(c(aucc1, aucc2, aucc3))
  
  weights <- 1/c(aucc1, aucc2)
  weights <- weights/sum(weights)
  
  score_df_sum[,topic] <- weights[1]*df$llm1_score + weights[2]*df$llm2_score
}

# aggregate score by PCA
score_df_pca <- score_df_gpt4o
for(topic in fea_df$fea){
  pca_df <- data.frame(llm1 = score_df_llama[,topic] , llm2 = score_df_mini[,topic])
  # do a pca on this dataframe, with dimention = 2, keep the first demention
  # pca_df<- pca_df[complete.cases(pca_df),]
  pca_result <- prcomp(pca_df, center = TRUE, scale. = TRUE)
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


# evaluation
eval_sum <- eval_llm(score_df_sum, label_df_gpt4o)
eval_sum$llm <- "weighted_sum_score"
eval_pred <- eval_llm(score_df_pca, label_df_gpt4o)
eval_pred$llm <- "pca1_score"
eval_mini <- eval_llm(label_df_mini, label_df_gpt4o)
eval_mini$llm <- "gpt4omini_label"
eval_mini_s <- eval_llm(score_df_mini, label_df_gpt4o)
eval_mini_s$llm <- "gpt4omini_score"
eval_llama <- eval_llm(label_df_llama, label_df_gpt4o)
eval_llama$llm <- "llama_label"
eval_llama_s <- eval_llm(score_df_llama, label_df_gpt4o)
eval_llama_s$llm <- "llama_score"


label_df_ls <- get_label_agreed_by_dfs(list("llama" = label_df_llama, "mini" = label_df_mini))
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
label_df_agreed <- merge(label_df_ls$agreed, info_df, by = "sm_id", all.x = TRUE)
eval_agree <- eval_llm(label_df_agreed, label_df_gpt4o)
eval_agree$llm <- "agreement"

# # adjust by PRC
# adjust_label_df_agreed <- adjust_label_by_agreement(
#   label_df = label_df_agreed,
#   score_df = score_df_pca,
#   true_label_df = label_df_gpt4o)
# eval_agree_adj <- eval_llm(adjust_label_df_agreed, label_df_gpt4o)
# eval_agree_adj$llm <- "agreement_adjust"


# # adjust by p025
# adjust_label_df_agreed <- label_df_agreed
# for(topic in fea_df$fea){
#   adjust_label_df_agreed[which(score_df_pca[,topic]<quantile(score_df_pca[which(score_df_pca[,topic]>0),topic], 0.025)),topic] <- 0
# }
# eval_agree_adj <- eval_llm(adjust_label_df_agreed, label_df_gpt4o)
# eval_agree_adj$llm <- "agreement_adjust"


eval_df <- rbind(eval_pred, eval_mini, eval_mini_s, eval_llama, eval_llama_s) 
eval_df <- rbind(eval_df,eval_agree)
eval_df <- rbind(eval_df,eval_agree_adj)


# Plot using ggplot2 with rotated x-axis labels
p1 <- ggplot(eval_df, aes(x = as.factor(topic), y = f1_score, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p2 <- ggplot(eval_df, aes(x = as.factor(topic), y = auprc, fill = llm)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggarrange(p1,p2, ncol=1)

cali_plot(score_df_pca$calorie, label_df_agreed$calorie, T)
