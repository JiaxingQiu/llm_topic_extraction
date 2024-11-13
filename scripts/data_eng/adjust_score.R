setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list = ls())
source("./scripts/data_eng/func_x_transform.R")
source("./scripts/data_eng/func_get_score_df.R")
source("./scripts/data_eng/func_plot_distribution.R")
library(dplyr)

mdl_name = "stella_en_1.5B_v5"
# mdl_name = "stella_en_400M_v5"
# mdl_name = "all-mpnet-base-v2"

fea_df <- read.csv("./data/fea_df.csv")
# fea_df <- fea_df[which(!fea_df$fea%in%c("nosocialeat", "thinspo", "fearcarb")),]
obj <- get_score_df("gpt_data", mdl_name)
label_df_gpt4omini <- obj$labeled_df
score_df_gpt4omini <- obj$scores_df
obj <- get_score_df("llama_data", mdl_name)
label_df_llama8b <- obj$labeled_df
score_df_llama8b <- obj$scores_df
obj <- get_score_df("gpt4o_data", mdl_name)
label_df_gpt4o <- obj$labeled_df
score_df_gpt4o <- obj$scores_df


# sanity check
plot_distribution(score_df_gpt4omini,
                  score_df_llama8b,
                  score_df_gpt4o)


# # ---- get the pearson correlation between two llms -----
# library(pROC)
# library(PRROC)
# 
# correlation_results <- list()
# correlation_results_raw <- list()
# auroc <- list()
# auroc_raw <- list()
# auprc <- list()
# auprc_raw <- list()
# for (topic in fea_df$fea) {
#   correlation <- cor(score_df_gpt4omini[[topic]], score_df_llama8b[[topic]], use = "complete.obs", method = "pearson")
#   correlation_raw <- cor(label_df_gpt4omini[[topic]], label_df_llama8b[[topic]], use = "complete.obs", method = "pearson")
#   correlation_results[[topic]] <- correlation
#   correlation_results_raw[[topic]] <- correlation_raw
#   
#   
#   
#   mdl_df <- data.frame(all_label_gpt4 = label_df_gpt4omini[[topic]],
#                        all_label_llama = label_df_llama8b[[topic]],
#                        all_score_gpt4 = score_df_gpt4omini[[topic]],
#                        all_score_llama = score_df_llama8b[[topic]])
#   mdl_df <- mdl_df[complete.cases(mdl_df),]
#   mdl_score <- glm(all_label_gpt4 ~ all_score_llama, data=mdl_df, family = binomial(link = "logit"))
#   predicted_probs <- predict(mdl_score, type = "response")
#   auroc[[topic]] <- round(auc(roc(mdl_df$all_label_gpt4, predicted_probs)),6)
#   pr_curve <- pr.curve(scores.class0 = predicted_probs, weights.class0 = mdl_df$all_label_gpt4 == 1, curve = TRUE)
#   auprc[[topic]] <- pr_curve$auc.integral
#   mdl_label <- glm(all_label_gpt4 ~ as.factor(all_label_llama), data=mdl_df, family = binomial(link = "logit"))
#   predicted_probs <- predict(mdl_label, type = "response")
#   auroc_raw[[topic]] <- round(auc(roc(mdl_df$all_label_gpt4, predicted_probs)),6)
#   pr_curve <- pr.curve(scores.class0 = predicted_probs, weights.class0 = mdl_df$all_label_gpt4 == 1, curve = TRUE)
#   auprc_raw[[topic]] <- pr_curve$auc.integral
#   
# }
# adjust_df <- data.frame(correlation_score = unlist(correlation_results), 
#                         correlation_label = unlist(correlation_results_raw),
#                         auprc_score = unlist(auprc), 
#                         auprc_label = unlist(auprc_raw))
# print(adjust_df)
# 
# all_score_gpt4 <- as.numeric(unlist(score_df_gpt4omini[,2:20]))
# all_score_llama <- as.numeric(unlist(score_df_llama8b[,2:20]))
# all_label_gpt4 <- as.numeric(unlist(label_df_gpt4omini[,4:22]))
# all_label_llama <- as.numeric(unlist(label_df_llama8b[,4:22]))
# correlation <- cor(all_score_gpt4, all_score_llama, use = "complete.obs", method = "pearson")
# correlation_raw <- cor(all_label_gpt4, all_label_llama, use = "complete.obs", method = "pearson")
# print(paste0("score corr = ", round(correlation,4),"  label corr = ", round(correlation_raw,4) ))
# 
# 
# 
# par(mfrow=c(2,2))
# plot(jitter(label_df_gpt4omini$bodyhate), jitter(label_df_llama8b$bodyhate))
# plot(score_df_gpt4omini$bodyhate, score_df_llama8b$bodyhate)
# # plot(all_score_gpt4, all_score_llama)


# # ----- use score of llama to predict label of gpt -----
# # topic = "fearfood"
# # all_score_gpt4 <- as.numeric(unlist(score_df_gpt4omini[,topic]))
# # all_score_llama <- as.numeric(unlist(score_df_llama8b[,topic]))
# # all_label_gpt4 <- as.numeric(unlist(label_df_gpt4omini[,topic]))
# # all_label_llama <- as.numeric(unlist(label_df_llama8b[,topic]))
# 
# library(pROC)
# library(PRROC)
# 
# mdl_df <- data.frame(all_label_gpt4 = all_label_gpt4,
#                      all_label_llama = all_label_llama,
#                      all_score_gpt4 = all_score_gpt4,
#                      all_score_llama = all_score_llama)
# mdl_df <- mdl_df[complete.cases(mdl_df),]
# # Fit the logistic regression model
# mdl_score <- glm(all_label_gpt4 ~ all_score_llama, data=mdl_df, family = binomial(link = "logit"))
# predicted_probs <- predict(mdl_score, type = "response")
# roc_curve <- roc(mdl_df$all_label_gpt4, predicted_probs)
# plot(roc_curve, main = paste("adjusted AUROC:", round(auc(roc_curve),6) ))
# # pr_curve <- pr.curve(scores.class0 = predicted_probs, weights.class0 = all_label_gpt4 == 1, curve = TRUE)
# # plot(pr_curve)
# 
# # Fit the logistic regression model
# mdl_label <- glm(all_label_gpt4 ~ as.factor(all_label_llama), data=mdl_df, family = binomial(link = "logit"))
# predicted_probs <- predict(mdl_label, type = "response")
# roc_curve <- roc(mdl_df$all_label_gpt4, predicted_probs)
# plot(roc_curve, main = paste("adjusted AUROC:", round(auc(roc_curve),6) ))
# # pr_curve <- pr.curve(scores.class0 = predicted_probs, weights.class0 = all_label_gpt4 == 1, curve = TRUE)
# # plot(pr_curve)
