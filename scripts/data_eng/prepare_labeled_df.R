setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

rm(list=ls())

library(dplyr)
library(parallel)
source("./scripts/data_eng/func_label_topics.R")
source("./scripts/data_eng/func_extract_phrases.R")
fea_df <- read.csv("./data/fea_df.csv")



# ------ 1. GPT 4o mini --------
# loop through all csv files under "./gpt_data" folder
folder_path <- "./gpt_data"
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
answer_df <- lapply(csv_files, read.csv) %>% bind_rows()  
answer_df$answer_string <- answer_df$responses
text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
answer_df <- merge(answer_df, text_df[,c("sm_id", "text_w_eos")], all.x=T)
answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
if(!file.exists("./gpt_data/results.rds")){
  res_list <- get_result_list(answer_df, fea_df$fea)
  saveRDS(res_list, "./gpt_data/results.rds")
}
# --- assign label to dataframe ---
labeled_df_gpt4omini <- format_answer_df(answer_df, fea_df$fea)
# res_list <- readRDS("./gpt_data/results.rds")
# answer_df[query_topics] <- NA
# topic_values <- mclapply(answer_df$sm_id, get_label, mc.cores = detectCores() - 2)
# answer_df[query_topics] <- do.call(rbind, topic_values)
# labeled_df_gpt4omini <- answer_df




# ------ 2. llama 8b -------
# answer_df_part1.csv contain first part of llama query results
answer_df_llama1 <- read.csv("./llama_data/answer_df_part1.csv", stringsAsFactors = F)
answer_df_llama2 <- read.csv("./llama_data/answer_df_part2.csv", stringsAsFactors = F)
# part 1
answer_df_llama1$answer_string <- gsub("thinspo","thinny",answer_df_llama1$answer_string)
answer_df_llama1$answer_string <- gsub("bodyhate","body",answer_df_llama1$answer_string)
answer_df <- answer_df_llama1 %>% filter( (!is.na(answer_string)) & (!answer_string=="") &
                                            (!is.na(answer_string2)) & (!answer_string2=="") &
                                            (!is.na(answer_string3)) & (!answer_string3=="") ) %>%
  as.data.frame()
answer_df$answer_string <- paste0(answer_df$answer_string, "\n", answer_df$answer_string2, "\n", answer_df$answer_string3 )
answer_df <- answer_df %>% select(sm_id, text_w_eos, answer_string) %>% as.data.frame()
# part 2
answer_df <- bind_rows(answer_df, answer_df_llama2)
answer_df <- answer_df[,c("sm_id", "text_w_eos", "answer_string")]
if(!file.exists("./llama_data/results.rds")){
  res_list <- get_result_list(answer_df, fea_df$fea)
  saveRDS(res_list, "./llama_data/results.rds")
}
# --- assign label to dataframe ---
labeled_df_llama8b <- format_answer_df(answer_df, fea_df$fea)
# res_list <- readRDS("./llama_data/results.rds")
# answer_df[query_topics] <- NA
# topic_values <- mclapply(answer_df$sm_id, get_label, mc.cores = detectCores() - 2)
# answer_df[query_topics] <- do.call(rbind, topic_values)
# labeled_df_llama8b <- answer_df




# ------ 3. viz distribution ------
# additional info
text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
info_df <- text_df %>% select(sm_id, group, sr_name, url)
labeled_df_gpt4omini <- merge(labeled_df_gpt4omini, info_df, by = "sm_id", all.x = TRUE)
labeled_df_llama8b <- merge(labeled_df_llama8b, info_df, by = "sm_id", all.x = TRUE)
rm(text_df)
# sanity check
distribution_gpt4omini <- labeled_df_gpt4omini %>%
  group_by(group) %>%
  summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
distribution_llama8b <- labeled_df_llama8b %>%
  group_by(group) %>%
  summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))

library(pheatmap)
tmp1 <- distribution_gpt4omini
tmp2 <- distribution_llama8b
mat1 <- as.matrix(tmp1[, -1])
rownames(mat1) <- tmp1$group
pheatmap(mat1,  # Remove the group column, if it's the first column
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         legend_breaks = seq(0,1,0.1),
         main = "Occurrence rates by GPT 4o-mini")
mat2 <- as.matrix(tmp2[, -1])
rownames(mat2) <- tmp2$group
pheatmap(mat2,  # Remove the group column, if it's the first column
         cluster_rows = F,
         cluster_cols = F,
         legend_breaks = seq(0,1,0.1),
         main = "Occurrence rates by Llama 8b-instruct (need a second run)")

# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
tmp1 <- distribution_gpt4omini %>% mutate(model = "GPT-4o-mini")
tmp2 <- distribution_llama8b %>% mutate(model = "Llama 8b-instruct")
combined_data <- bind_rows(tmp1, tmp2)
long_data <- combined_data  %>%
  pivot_longer(cols = -c(group, model), names_to = "category", values_to = "occurrence_rate")
ggplot(long_data, aes(x = group, y = occurrence_rate, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~category,ncol=4, scales = "free_x") + 
  labs(y = "Occurrence Rate") +
  theme_minimal() +
  scale_fill_manual(values = c("GPT-4o-mini" = "skyblue", "Llama 8b-instruct" = "coral")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")


# # ------- 4. sample human label data --------
# labeled_df_gpt4omini <- merge(labeled_df_gpt4omini, labeled_df_llama8b[,c("sm_id","text_w_eos")])
# tmp <- labeled_df_gpt4omini %>% 
#   group_by(sr_name) %>% 
#   summarise(nsm = n_distinct(sm_id)) %>%
#   filter(nsm>=10) %>% as.data.frame() # 
# labeled_df <- labeled_df_gpt4omini %>% filter(sr_name %in% tmp$sr_name)
# print(setdiff(unique(labeled_df_gpt4omini$sr_name),tmp$sr_name)) # remember to remove these forums from semantic network
# set.seed(333)
# sampled_df <- labeled_df %>%
#   group_by(sr_name) %>%
#   sample_n(size = 10) %>%
#   ungroup()  # Remove grouping for further analysis
# print(sampled_df$sm_id)
# sampled_df_human <- sampled_df[,c("sm_id","text_w_eos","answer_string")]
# sampled_df_human$answer_string_human <- paste0(paste0(fea_df$fea, collapse = ": 0\n"), ": 0\n")
# write.csv(sampled_df_human, "human_data/sampled_posts.csv", row.names = F)


