library(dplyr)
library(stringr)


# raw_text = answer_df[answer_df$sm_id=="1003i3b","text_w_eos"]
# answer_string = answer_df[answer_df$sm_id=="1003i3b","answer_string"]
# query_topics = fea_df$fea
func_extract_phrases <- function(answer_string, query_topics, raw_text){
  
  answer_string <- gsub("\\[yes\\]", "yes", answer_string)
  answer_string <- gsub("\\[no\\]", "no", answer_string)
  answer_string <- gsub("Total time: \\d+(.*?)\\d+ sec.", "", answer_string)
  answer_string <- gsub("\\n ", "\\\n", answer_string)
  answer_string <- paste0(answer_string, "\n") # add a \n
  
  
  res <- list()
  for (i in seq_along(query_topics)) {
    
    topic <- query_topics[i]
    res[[topic]] <- list()
    res[[topic]][['label']] <- NA
    res[[topic]][['phrases']] <- list()
    
    
    pattern <- paste0("\\)\\s*", topic, ":\\s*(yes|no)")
    if (grepl(pattern, answer_string)) {
      
      # -----  1. get label ----- 
      # Extract the matched part of the string that contains "yes" or "no"
      match <- regmatches(answer_string, regexpr(pattern, answer_string))
      label <- regmatches(match, regexpr("\\b(yes|no)\\b", match))
      res[[topic]][['label']] <- ifelse(label == "yes", 1, 0)
      
      # -----  2. get phrases ----- 
      # find the string in answer_string after paste0(query_topics[i], ":\\s*(yes|no)"), before "\n(\\d+"
      pattern <- paste0("\\)\\s*", topic, ":\\s*(yes|no)(.*?)\\n\\s*\\(\\d+")
      matches <- str_match(answer_string, pattern)
      if(is.na(matches[,2])){ # try if the topic is at the end of query
        pattern <- paste0("\\)\\s*", topic, ":\\s*(yes|no)(.*?)\\n\\s*")
        matches <- str_match(answer_string, pattern)
      }
      if(matches[,2] == "yes"){
        phrases_string <- matches[, 3]
        # Split phrases_string by one or more punctuation characters
        split_phrases <- unlist(strsplit(phrases_string, "[[:punct:]]+"))
        # Remove strings that contain the word "phrases"
        split_phrases <- split_phrases[!grepl("phrases", split_phrases, ignore.case = TRUE)]
        # Remove blank or empty strings, remove strings only contain blanks
        split_phrases <- Filter(nzchar, trimws(split_phrases))
        # to lower case 
        split_phrases <- unlist(lapply(split_phrases, function(phrase) tolower(phrase)))
        # for each element, check whether the string appears in the raw_text
        matched_phrases <- sapply(split_phrases, function(phrase) grepl(phrase, raw_text))
        
        res[[topic]][['phrases']] <- split_phrases[matched_phrases]
      }
    }
  }
  
  return(res)
}




# Function to format the answer dataframe using apply
get_result_df <- function(answer_df, query_topics) {
  
  # make sure "text_w_eos", "answer_string", "sm_id" are in answer_df
  stopifnot("text_w_eos" %in% colnames(answer_df) )
  stopifnot("answer_string" %in% colnames(answer_df) )
  stopifnot("sm_id" %in% colnames(answer_df) )
  
  answer_df[query_topics] <- NA
  answer_df[paste0(query_topics,"_phrases")] <- NA
  
  
  # Helper function to process each row
  process_row <- function(sm_id) {
    res_list <- list()
    raw_text <- answer_df[answer_df$sm_id==sm_id,"text_w_eos"]
    answer_string <- answer_df[answer_df$sm_id==sm_id,"answer_string"]
    res_list[[sm_id]] <- func_extract_phrases(answer_string, query_topics, raw_text )
    
    
    # return(res_list)
    # Return a named list where topic columns are set to 1 or 0 based on topics01
    labels = sapply(query_topics, function(topic) {return(res_list[[sm_id]][[topic]]$label)})
    phrases = sapply(query_topics, function(topic) {return(paste0(res_list[[sm_id]][[topic]]$phrases,collapse = "; ") )})
    names(phrases) <- paste0(names(phrases),"_phrases")
    res <- c(labels, phrases)
    return(res)
  }
  
  # Apply the processing function to each row in answer_string and bind the result
  # library(parallel)
  # result_lists <- t(mclapply(answer_df$sm_id, process_row, mc.cores = detectCores() - 2))
  # # concate lists into one
  # combined_result <- do.call(c, result_lists)
  
  library(parallel)
  topic_res <- t(mclapply(answer_df$sm_id, process_row, mc.cores = detectCores() - 2))
  # Bind the result to the original answer_df
  answer_df[c(query_topics,paste0(query_topics,"_phrases"))] <- do.call(rbind, topic_res)
  
  return(answer_df)
}


# Define the function to get labels for each sm_id
get_label <- function(sm_id) {
  labels <- sapply(fea_df$fea, function(topic) {
    if (!is.null(res_list[[sm_id]][[topic]]$label)) {
      res_list[[sm_id]][[topic]]$label
    } else {
      NA  # Handle missing or null labels
    }
  })
  names(labels) <- fea_df$fea
  return(labels)
}
