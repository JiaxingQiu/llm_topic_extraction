setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

# read in excel 
library(readxl)
data <- read_excel("./data2/stigma.xlsx")
data$human_label01 <- ifelse(tolower(data$human_label)=="yes",1,0)
#coding_cat <- paste0(unique(data$`Coding Category`),collapse = ";")
fea <- c("WeightBasedHealthAssumptions",
         "WeightStigma",
         "WeightBasedHealthcare",
         "EncourageWeightLoss",
         "DietPromotion",
         "Thinness",
         "NotLowWeightEnough",
         "WeightBlame",
         "WeightValue")
description <- c("Judgments or assumptions about health based on weight or appearance.",
                 "General weight-related discrimination or bias.",
                 "Healthcare decisions influenced by weight. Not taken seriously because of weight, or inadequate care due to weight.",
                 "Encouragement of weight loss.",
                 "Advocacy for diets or restrictive eating.",
                 "Reassurance of thinness. Maintaining thinness.",
                 "Not sick enough, or not thin enought, or not lose weight enough.",
                 "Weight blamed for health issues or concerns",
                 "Weight tied to personality characteristics.")

fea_df <- data.frame(fea, description)
write.csv(fea_df, "./data2/fea_df.csv", row.names = F)

answer_df <- data.frame("sm_id" = data$record_id,
                        "text_w_eos" = data$damaging_comment)
write.csv(answer_df, "./data2/answer_df_raw.csv", row.names = F)

