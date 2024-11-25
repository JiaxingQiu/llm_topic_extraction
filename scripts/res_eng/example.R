setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

# example: 104a474
source("./scripts/data_eng/prepare_analysis.R")

label_df_mistral[which(label_df_mistral$sm_id=="104a474"),]


