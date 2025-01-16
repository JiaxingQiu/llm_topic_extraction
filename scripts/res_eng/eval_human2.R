setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
library(dplyr)
library(readxl)

# table subject and narratives
df <- read_excel("./data2/raw/stigma.xlsx")
n_distinct(df$record_id)
length(df$record_id)


# read in excel 
data <- read_excel("./data2/raw/coders2.xlsx")
data$record_id <- as.numeric(data$record_id)
data$coder1 <- ifelse(tolower(data$`Weight Stigma? Noelle`)=="yes",1,0)
data$coder2 <- ifelse(tolower(data$`Weight Stigma? Khush`)=="yes",1,0)
data$coder3 <- ifelse(as.numeric(data$`Weight Stigma? Natalie`)==1,1,0)
data$consensus <- tolower(data$`Consensus [Yes, No, No Consensus]`)
data$coder2[which(data$consensus == "yes")] <- 1
data$coder2[which(data$consensus == "no")] <- 0

label_data <- data.frame("coder1" = as.numeric(data$coder1),
                         "coder3" = as.numeric(data$coder3))
label_data <- label_data[complete.cases(label_data),]
table(label_data)
irrCAC::fleiss.kappa.raw(label_data)$est
irrCAC::gwet.ac1.raw(label_data)$est


# --- overall level of agreement across 4 llms ----
source("./scripts/data_eng/prepare_analysis2.R")
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


