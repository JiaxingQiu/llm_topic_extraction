setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

library(dplyr)
fea_df <- read.csv("./data/fea_df.csv")
rownames(fea_df) <- fea_df$fea
fea_df <- fea_df[setdiff(fea_df$fea, c("nosocialeat", "thinspo", "leanbody", "fearcarb","meal")),]
fea_df <- fea_df[sort(fea_df$fea),]
rownames(fea_df) <- NULL
fea_df <- bind_rows(fea_df,data.frame("fea" = "idealbody",
                                      "feature" = "Ideal body image",
                                      "description" = "Ideal body image including thinness, skinny body, low body fat and lean body mass."))

# --- gpt4o vs human ---
source("./scripts/data_eng/prepare_analysis.R")
label_true <- read.csv("./human_data/results.csv")
label_df_gpt4o <- label_df_gpt4o[which(label_df_gpt4o$sm_id %in% label_true$sm_id),]
label_data <- data.frame("human" = as.numeric(unlist(label_true[,fea_df$fea])),
                         "gpt4o" = as.numeric(unlist(label_df_gpt4o[,fea_df$fea])))
irrCAC::fleiss.kappa.raw(label_data)$est
irrCAC::gwet.ac1.raw(label_data)$est


# --- between human ---
label1 <- read.csv("./human_data/results.csv")
label2 <- read.csv("./human_data/results2.csv")
label1 <- label1[which(label1$sm_id%in%label2$sm_id),]
label_data <- data.frame("human1" = as.numeric(unlist(label1[,fea_df$fea])),
                         "human2" = as.numeric(unlist(label2[,fea_df$fea])))
irrCAC::fleiss.kappa.raw(label_data)$est
irrCAC::gwet.ac1.raw(label_data)$est



# --- overall level of agreement across 4 llms ----
source("./scripts/data_eng/prepare_analysis.R")
# label_true <- read.csv("./human_data/results.csv")
# label_df_llama <- label_df_llama[which(label_df_llama$sm_id %in% label_true$sm_id),]
# label_df_mistral <- label_df_mistral[which(label_df_mistral$sm_id %in% label_true$sm_id),]
# label_df_vicuna7b <- label_df_vicuna7b[which(label_df_vicuna7b$sm_id %in% label_true$sm_id),]
# label_df_qwen <- label_df_qwen[which(label_df_qwen$sm_id %in% label_true$sm_id),]
label_data <- data.frame("llama" = as.numeric(unlist(label_df_llama[,fea_df$fea])),
                         "mistral" = as.numeric(unlist(label_df_mistral[,fea_df$fea])),
                         "vicuna7b" = as.numeric(unlist(label_df_vicuna7b[,fea_df$fea])),
                         "qwen" = as.numeric(unlist(label_df_qwen[,fea_df$fea])) )
irrCAC::fleiss.kappa.raw(label_data)$est
irrCAC::gwet.ac1.raw(label_data)$est


# overall level of 