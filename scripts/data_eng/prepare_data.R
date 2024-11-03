
# 1. answer_df
# run "~/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/data_clean_in_r_with_EOS.R" first
df_llm <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = F)

answer_df <- df_llm[,c("sm_id", "text_w_eos")]

write.csv(answer_df, "/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/answer_df_raw.csv", row.names = F)



# 2. fea_df
# run "~/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/prepare_fea_df.R"
source("~/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/prepare_fea_df.R")
write.csv(fea_df, "/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/fea_df.csv", row.names = F)



