setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
library(irrCAC)
library(dplyr)
library(knitr)
library(kableExtra)

format_table <- function(ac1_data,
                         type=c("ac1", "fleiss")[1]){
  library(dplyr)
  ac1_data$label <- paste0(sprintf("%1.3f", ac1_data$label_est), 
                           " [",sprintf("%1.3f", ac1_data$label_ci_low),", ", sprintf("%1.3f", ac1_data$label_ci_up),"]")
  ac1_data$score <- paste0(sprintf("%1.3f", ac1_data$score_est), 
                           " [",sprintf("%1.3f", ac1_data$score_ci_low),", ", sprintf("%1.3f", ac1_data$score_ci_up),"]")
  ac1_data$higher <- NA
  ac1_data$higher[which(ac1_data$label_est > ac1_data$score_ci_up)] <- "label"
  ac1_data$higher[which(ac1_data$score_est > ac1_data$label_ci_up)] <- "score"
  ac1_data <- ac1_data %>% arrange(label_est)
  ac1_data <- ac1_data[,c("topic","label", "score", "higher")]
  # Function to apply conditional bold formatting
  bold_first_five <- function(score, condition) {
    if (condition) {
      return(paste0("\\textbf{", substr(score, 1, 5), "}", substr(score, 6, nchar(score))))
    } else {
      return(score)
    }
  }
  ac1_data <- ac1_data %>%
    mutate(score = ifelse(higher == "score" & (!is.na(higher)), 
                          bold_first_five(score, TRUE), 
                          score))
  ac1_data <- ac1_data[,c("topic", "label", "score")]
  colnames(ac1_data) <- c("topic", paste0(c("label", "score"), "_", type))
  return(ac1_data)
}

eval_agree <- function(weights = "unweighted",
                       add_llm = c(),
                       rm_llm = c(),
                       type=c("ac1", "fleiss")[1]){
  res_df <- data.frame()
  for(topic in fea_df$fea){
    label_data <- data.frame("llama" = label_df_llama[[topic]],
                             "mistral" = label_df_mistral[[topic]],
                             "qwen" = label_df_qwen[[topic]],
                             "vicuna7b" = label_df_vicuna7b[[topic]])
    for(llm in add_llm){
      if(llm == "mini") label_data[["mini"]] <- label_df_mini[[topic]]
      if(llm == "gpt") label_data[["mini"]] <- label_df_gpt4o[[topic]]
    }
    label_data <- label_data[,setdiff(colnames(label_data), rm_llm)]
    
    if(type=="fleiss"){
      label_ac1 <- irrCAC::fleiss.kappa.raw(label_data)$est
    }else{
      label_ac1 <- irrCAC::gwet.ac1.raw(label_data)$est
    }
    label_ac1_ci <- gsub("\\)","",gsub("\\(","",label_ac1$conf.int))
    label_ac1_ci_low <- as.numeric(unlist(strsplit(label_ac1_ci,","))[1])
    label_ac1_ci_up <- as.numeric(unlist(strsplit(label_ac1_ci,","))[2])
    
    
    score_data <- data.frame("llama" = score_df_llama[[topic]],
                             "mistral" = score_df_mistral[[topic]],
                             "qwen" = score_df_qwen[[topic]],
                             "vicuna7b" = score_df_vicuna7b[[topic]])
    for(llm in add_llm){
      if(llm == "mini") score_data[["mini"]] <- score_df_mini[[topic]]
      if(llm == "gpt") score_data[["mini"]] <- score_df_gpt4o[[topic]]
    }
    score_data <- score_data[,setdiff(colnames(score_data), rm_llm)]
    
    score_data <- data.frame(lapply(score_data, function(col) {
      cut(col, breaks = seq(0, 1, by = 0.1), labels = FALSE, include.lowest = TRUE)
    }))
    if(type=="fleiss"){
      score_ac1 <- irrCAC::fleiss.kappa.raw(score_data, weights = weights)$est
    }else{
      score_ac1 <- irrCAC::gwet.ac1.raw(score_data, weights = weights)$est
    }
    score_ac1_ci <- gsub("\\)","",gsub("\\(","",score_ac1$conf.int))
    score_ac1_ci_low <- as.numeric(unlist(strsplit(score_ac1_ci,","))[1])
    score_ac1_ci_up <- as.numeric(unlist(strsplit(score_ac1_ci,","))[2])
    
    res_df <- bind_rows(res_df, 
                        data.frame("topic" = topic, 
               "label_est" = round(label_ac1$coeff.val,3),
               "label_ci_low" = label_ac1_ci_low,
               "label_ci_up" = label_ac1_ci_up,
               "score_est" = round(score_ac1$coeff.val,3),
               "score_ci_low" = score_ac1_ci_low,
               "score_ci_up" = score_ac1_ci_up ))
  }
  return(res_df)
}

# ------ data 1 --------
source("./scripts/data_eng/prepare_analysis.R")
ac1_data1 <- eval_agree(type = "ac1")
ac_df1 <- format_table(ac1_data1, type="ac1")
kappa_data1 <- eval_agree(weights = "ordinal",type = "fleiss")
kappa_df1 <- format_table(kappa_data1, type="fleiss")

# ------ data 2 --------
source("./scripts/data_eng/prepare_analysis2.R")
ac1_data2 <- eval_agree(type = "ac1")
ac_df2 <- format_table(ac1_data2, type="ac1")
kappa_data2 <- eval_agree(weights = "ordinal",type = "fleiss")
kappa_df2 <- format_table(kappa_data2, type="fleiss")


df1 <- merge(ac_df1, kappa_df1, by="topic", all.x=T) %>% arrange(topic)
df2 <- merge(ac_df2, kappa_df2, by="topic", all.x=T) %>% arrange(topic)
df2$topic <- "weightstigma"


# Generate LaTeX table
kable(df1,
      col.names = c("Topic", "Labels", "Scores", "Labels", "Scores"),
      format = "latex", booktabs = TRUE, escape = FALSE) %>%
  add_header_above(c(" ", "Gwet's AC1"=2,  "Fleiss' kappa"=2))%>%  
  kable_styling(full_width = FALSE) %>% 
  column_spec(1, width = "0.9in") %>%  # Set width for the first column
  column_spec(2, width = "1.4in") %>%  # Set width for the second column
  column_spec(3, width = "1.4in")  %>% # Set width for the third column
  column_spec(4, width = "1.4in") %>%  # Set width for the second column
  column_spec(5, width = "1.4in")  %>% # Set width for the third column
  kable_minimal(full_width = F)






