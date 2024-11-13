setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

mdl_name = "stella_en_1.5B_v5"
# mdl_name = "stella_en_400M_v5"
# mdl_name = "all-mpnet-base-v2"

# topic = "feargain"#"bodyhate"
# sm_id = "1003i3b"
get_cos_score <- function(topic, sm_id) {
  # llm label 0 or 1
  label_topic <- labeled_df[which(labeled_df$sm_id==sm_id),topic]
  # string contain cosine scores
  cos_string <- labeled_df[which(labeled_df$sm_id==sm_id),paste0(topic,"_phrases")]
  cos_elements <- unlist(strsplit(cos_string, ";")) # Split cos_string by ";"
  cos_elements <- Filter(nzchar, trimws(cos_elements)) # remove empty string
  
  # initial value of cos_score is -1
  cos_score <- -1
  # if there are existed phrases, get the cos_score
  if(length(cos_elements)>0){
    # Extract the number inside parentheses for each element
    cos_values <- sapply(cos_elements, function(element) {
      # Use regex to find the number inside parentheses
      matches <- regmatches(element, regexpr("\\(([^)]+)\\)", element))
      as.numeric(gsub("[()]", "", matches))  # Convert to numeric after removing parentheses
    })
    cos_score <- max(cos_values, na.rm = TRUE)
  }
  return(cos_score)
}



for(folder in c("gpt_data", "gpt4o_data", "llama_data")){
  
  cos_score_path <- paste0("./",folder,"/cos_score_",mdl_name,".RDS")
  
  if(!file.exists(cos_score_path)){
    
    # read embedding adjusted data
    fea_df <- read.csv("./data/fea_df.csv")
    labeled_df <- read.csv(paste0("./",folder,"/results_with_cos_",mdl_name,".csv"))
    labeled_df <- labeled_df[,setdiff(colnames(labeled_df), "X")]
    
    # apply the get_cos_score function to each topic for each sm_id
    library(parallel)
    process_row <- function(sm_id) {
      res <- sapply(fea_df$fea, function(topic) get_cos_score(topic, sm_id))
      return(res)
    }
    scores <- t(mclapply(labeled_df$sm_id, process_row, mc.cores = detectCores() - 2))
    scores_df <- do.call(rbind, scores)
    scores_df <- as.data.frame(scores_df)
    scores_df$sm_id <- labeled_df$sm_id
    saveRDS(scores_df, cos_score_path)
  }
}



