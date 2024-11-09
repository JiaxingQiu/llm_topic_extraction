
# 1. answer_df
# run "~/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/data_clean_in_r_with_EOS.R" first
df_llm <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = F)

answer_df <- df_llm[,c("sm_id", "text_w_eos")]
# answer_df1 <- answer_df[1:30000,]
# answer_df2 <- answer_df[30001:nrow(answer_df),]
# stopifnot(nrow(answer_df2) + nrow(answer_df1) == nrow(answer_df))
# setdiff(union(answer_df2$sm_id, answer_df1$sm_id), answer_df$sm_id)
# setdiff(answer_df$sm_id, union(answer_df2$sm_id, answer_df1$sm_id))
# 
# write.csv(answer_df1, "/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/answer_df_raw1.csv", row.names = F)
# write.csv(answer_df2, "/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/answer_df_raw2.csv", row.names = F)
write.csv(answer_df, "/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/answer_df_raw.csv", row.names = F)



# 1.2 answer_df part 2 for llama 8b
setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
# answer_df_part1.csv contain first part of llama query results
answer_df_llama1 <- read.csv("./llama_data/answer_df_part1.csv", stringsAsFactors = F)
# part 3 is the second run of previous part 1
if(!file.exists("./llama_data/answer_df_part3.csv")){
  answer_df_part1_raw <- answer_df_llama1[,c("sm_id","text_w_eos")]
  write.csv(answer_df_part1_raw,"./llama_data/answer_df_part3.csv", row.names = F)
}

if(!file.exists("./llama_data/answer_df_part2.csv")){
  answer_df_raw <- read.csv("./data/answer_df_raw.csv", stringsAsFactors = F)
  remain_sm <- setdiff(answer_df_raw$sm_id, answer_df_llama1$sm_id)
  remain_df <- answer_df_raw[which(answer_df_raw$sm_id %in% remain_sm),]
  write.csv(remain_df,"./llama_data/answer_df_part2.csv", row.names = F)
  # run llama on answer_df_raw_part2.csv, using fea_df
  rm(answer_df_raw, remain_df)
}


# 2. fea_df
# run "~/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/prepare_fea_df.R"
source("~/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/prepare_fea_df.R")
write.csv(fea_df, "/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/fea_df.csv", row.names = F)



