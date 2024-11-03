setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

rm(list=ls())
source("./scripts/func_label_topics.R")
fea_df <- read.csv("./data/fea_df.csv")

# answer_df_part1.csv contain first part of llama query results
answer_df_llama1 <- read.csv("./llama_data/answer_df_part1.csv", stringsAsFactors = F)
if(!file.exists("./llama_data/answer_df_part2.csv")){
  answer_df_raw <- read.csv("./data/answer_df_raw.csv", stringsAsFactors = F)
  remain_sm <- setdiff(answer_df_raw$sm_id, answer_df_llama1$sm_id)
  remain_df <- answer_df_raw[which(answer_df_raw$sm_id %in% remain_sm),]
  write.csv(remain_df,"./llama_data/answer_df_part2.csv", row.names = F)
  # run llama on answer_df_raw_part2.csv, using fea_df
  rm(answer_df_raw, remain_df)
}else{
  answer_df_llama2 <- read.csv("./llama_data/answer_df_part2.csv", stringsAsFactors = F)
}

# first part 
answer_df_llama1$answer_string <- gsub("thinspo","thinny",answer_df_llama1$answer_string)
answer_df <- answer_df_llama1 %>% filter( (!is.na(answer_string)) & (!answer_string=="") &
                                     (!is.na(answer_string2)) & (!answer_string2=="") &
                                       (!is.na(answer_string3)) & (!answer_string3=="") ) %>%
  as.data.frame()
answer_df$answer_string <- paste0(answer_df$answer_string, "\n", answer_df$answer_string2, "\n", answer_df$answer_string3 )
answer_df <- answer_df %>% select(sm_id, text_w_eos, answer_string) %>% as.data.frame()
labeled_df <- format_answer_df(answer_df, fea_df$fea)
labeled_df <- labeled_df[complete.cases(labeled_df),]

# # sanity check
# text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
# info_df <- text_df %>% select(sm_id, group, sr_name)
# rm(text_df)
# labeled_df <- merge(labeled_df, info_df, by = "sm_id", all.x = TRUE)
# tmp<-labeled_df %>%
#   group_by(group) %>%
#   summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
# View(tmp)
# print(labeled_df$answer_string[33])


