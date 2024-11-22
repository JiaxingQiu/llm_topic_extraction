library(dplyr)
library(parallel)
library(ggplot2)
library(ggpubr)

path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)


# prepare label_df, score_df
obj <- get_score_df("gpt4o_data")
score_df_gpt4o <- obj$scores_df
label_df_gpt4o <- obj$labeled_df
obj <- get_score_df("gpt_data")
score_df_mini <- obj$scores_df
label_df_mini <- obj$labeled_df
obj <- get_score_df("llama_data")
score_df_llama <- obj$scores_df
label_df_llama <- obj$labeled_df
obj <- get_score_df("qwen_data")
score_df_qwen <- obj$scores_df
label_df_qwen <- obj$labeled_df
obj <- get_score_df("vicuna13b_data")
score_df_vicuna13b <- obj$scores_df
label_df_vicuna13b <- obj$labeled_df
obj <- get_score_df("vicuna7b_data")
score_df_vicuna7b <- obj$scores_df
label_df_vicuna7b <- obj$labeled_df
obj <- get_score_df("mistral_data")
score_df_mistral <- obj$scores_df
label_df_mistral <- obj$labeled_df
rm(obj)
rm_feas <- c("fearcarb")
fea_df <- read.csv("./data/fea_df.csv")
fea_df <- fea_df[which(!fea_df$fea%in%rm_feas),]

# ---- sanity check ----
label_df_ls = list(label_df_gpt4o,
                   label_df_mini,
                   label_df_llama,
                   label_df_qwen,
                   label_df_vicuna13b,
                   label_df_vicuna7b,
                   label_df_mistral)
score_df_ls = list(score_df_gpt4o,
                   score_df_mini,
                   score_df_llama,
                   score_df_qwen,
                   score_df_vicuna13b,
                   score_df_vicuna7b,
                   score_df_mistral)
llm_name_ls = list("GPT4o",
                   "GPT4o-mini",
                   "Llama8b",
                   "Qwen7b",
                   "Vicuna13b",
                   "Vicuna7b",
                   "Mistral")
plot_distribution(label_df_ls,
                  llm_name_ls)
plot_distribution(score_df_ls,
                  paste0(llm_name_ls,"_score") )

# topic = "calorie"
# # make a calibration curve
# cali_plot(score_df_llama[[topic]], label_df_llama[[topic]], plot = T)
# cali_plot(score_df_mini[[topic]], label_df_mini[[topic]], plot = T)
# cali_plot(score_df_qwen[[topic]], label_df_qwen[[topic]], plot = T)
# cali_plot(score_df_gpt4o[[topic]], score_df_gpt4o[[topic]], plot = T)


