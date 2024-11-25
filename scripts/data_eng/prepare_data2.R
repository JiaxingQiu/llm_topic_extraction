setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

library(dplyr)
library(parallel)
library(readxl)

path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)



# read in excel 
data <- read_excel("./data2/raw/stigma.xlsx")


#coding_cat <- paste0(unique(data$`Coding Category`),collapse = ";")
fea <- tolower(c("WeightBasedHealth",
         "WeightStigma",
         "WeightBasedHealthcare",
         "EncourageWeightLoss",
         "DietPromotion",
         "Thinness",
         "NotLowWeightEnough",
         "WeightBlame",
         "WeightValue"))
description <- c("Judgments about health based on weight or appearance",
                 "Negative attitudes, discrimination, or prejudice based on body weight or size",
                 "Healthcare decisions influenced by weight. Not taken seriously because of weight, or inadequate care due to weight",
                 "Encouragement of weight loss",
                 "Advocacy for diets or restrictive eating",
                 "Reassurance of thinness. Maintaining thinness",
                 "Not sick enough, or not thin enought, or not lose weight enough",
                 "Weight blamed for health issues or concerns",
                 "Weight tied to personality characteristics")
fea_df <- data.frame("fea" = fea, "description" = description)
write.csv(fea_df, "./data2/fea_df.csv", row.names = F)


# text
answer_df <- data.frame("sm_id" = data$record_id,
                        "text_w_eos" = data$damaging_comment)
answer_df$text_w_eos <- sapply(answer_df$text_w_eos, clean_text)
write.csv(answer_df, "./data2/answer_df_llama.csv", row.names = F)
write.csv(answer_df, "./data2/answer_df_qwen.csv", row.names = F)
write.csv(answer_df, "./data2/answer_df_mistral.csv", row.names = F)
write.csv(answer_df, "./data2/answer_df_vicuna.csv", row.names = F)

answer_df$stigma <- ifelse(tolower(data$human_label)=="yes", 1, 0)
answer_df$pat_id <- answer_df$sm_id
answer_df$sm_id <- 1:nrow(answer_df)
write.csv(answer_df, "./data2/answer_df_raw.csv", row.names = F)




