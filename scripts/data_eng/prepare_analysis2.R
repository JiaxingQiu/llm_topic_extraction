library(dplyr)
library(parallel)
library(ggplot2)
library(ggpubr)

path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)


# prepare label_df, score_df
useweightstigma <- F
obj <- get_score_df2("llama", useweightstigma=useweightstigma)
score_df_llama <- obj$scores_df
label_df_llama <- obj$labeled_df
obj <- get_score_df2("qwen", useweightstigma=useweightstigma)
score_df_qwen <- obj$scores_df
label_df_qwen <- obj$labeled_df
obj <- get_score_df2("vicuna", useweightstigma=useweightstigma)
score_df_vicuna7b <- obj$scores_df
label_df_vicuna7b <- obj$labeled_df
obj <- get_score_df2("mistral", useweightstigma=useweightstigma)
score_df_mistral <- obj$scores_df
label_df_mistral <- obj$labeled_df
rm(obj)

fea_df <- read.csv("./data2/fea_df.csv")
fea_df <- data.frame("fea" = "stigma", "description" = fea_df$description[which(fea_df$fea=="weightstigma")])


# --- print correlation matrix ---
topic = "stigma"
# make a calibration curve
tmp <- cali_plot(score_df_llama[[topic]], label_df_llama[[topic]], plot = T)
tmp <- cali_plot(score_df_mistral[[topic]], label_df_mistral[[topic]], plot = T)
tmp <- cali_plot(score_df_qwen[[topic]], label_df_qwen[[topic]], plot = T)
tmp <- cali_plot(score_df_vicuna7b[[topic]], label_df_vicuna7b[[topic]], plot = T)
df <- data.frame(
  llama = score_df_llama[,topic],
  qwen = score_df_qwen[,topic],
  mistral = score_df_mistral[,topic],
  vicuna7b = score_df_vicuna7b[,topic]
)
correlation_matrix <- cor(df, use = "complete.obs")
# print(correlation_matrix)
# fea_df$fea
print(data.frame(
  llama = mean(label_df_llama[,topic]),
  qwen = mean(label_df_qwen[,topic]),
  mistral = mean(label_df_mistral[,topic]),
  vicuna7b = mean(label_df_vicuna7b[,topic])
))



