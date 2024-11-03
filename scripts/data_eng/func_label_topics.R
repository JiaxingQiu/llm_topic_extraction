library(dplyr)
library(stringr)

# Function to format the answer string
format_answer <- function(answer_string, query_topics) {
  
  answer_string <- gsub("\\[yes\\]", "yes", answer_string)
  answer_string <- gsub("\\[no\\]", "no", answer_string)
  
  
  topics01 <- list()
  for (i in seq_along(query_topics)) {
    topics01[[query_topics[i]]] <- NA
    pattern <- paste0("\\)\\s*", query_topics[i], ":\\s*(yes|no)")
    if (grepl(pattern, answer_string)) {
      # Extract the matched part of the string that contains "yes" or "no"
      match <- regmatches(answer_string, regexpr(pattern, answer_string))
      label <- regmatches(match, regexpr("\\b(yes|no)\\b", match))
      topics01[[query_topics[i]]] <- ifelse(label == "yes", 1, 0)
    }
  }
  
  
  
  # # To extract each topic (the word after (number)), the "yes" or "no" label, and the "related phrases" that come after the colon (:) but before the next (number) or the end of the string
  # pattern <- "\\((\\d+)\\)\\s*([\\w\\s]+):\\s*\\[(yes|no)\\](?:,\\s*related phrases\\s*'([^']*(?:',\\s*'[^']*)*)')?"
  # # pattern <- "\\((\\d+)\\)\\s*([\\w\\s]+):\\s*\\[(yes|no)\\](?:,\\s*([^\\(\\n]+))?"
  # # pattern <- "\\((\\d+)\\)\\s*([\\w\\s]+):\\s*\\[(yes|no)\\]\\s*\n"
  # 
  # # Extract data using str_match_all
  # matches <- str_match_all(answer_string, pattern)[[1]]
  # 
  # # Initialize empty lists to store the results
  # topics_detail <- list()
  # topics01 <- list()
  # 
  # for (i in seq_len(nrow(matches))) {
  #   index <- matches[i, 2]
  #   topic <- str_trim(matches[i, 3])
  #   if (topic == "body") {
  #     topic <- "bodyhate"
  #   }
  #   label <- str_trim(matches[i, 4])
  #   phrases <- if (!is.na(matches[i, 5])) str_split(matches[i, 5], "', '")[[1]] else character(0)
  #   # phrases <- if (!is.na(matches[i, 6])) str_split(matches[i, 6], "', '")[[1]] else character(0)
  #   phrases <- str_replace_all(phrases, "^'|'$", "")  # Clean quotes
  #   
  #   # Append details to topics_detail list
  #   topics_detail[[i]] <- list(
  #     Index = index,
  #     Topic = topic,
  #     Label = label,
  #     `Related Phrases` = phrases
  #   )
  #   
  #   # Set topics01 values to 1 or 0 based on label
  #   topics01[[topic]] <- ifelse(label == "yes", 1, 0)
  # }
  
  return(list(topics01 = topics01))
}
# # example usage
# answer_string <- "(1) nosocialeat: [yes], related phrases: hhhh\n(2) depressedmood: [no], related phrases: none\n(3) body: [no]\n"
# format_answer(answer_string)
# answer_string <- answer_df$answer_string[which(answer_df$sm_id=="133sy7i")] # 
# query_topics <- fea_df$fea



# Function to format the answer dataframe using apply
format_answer_df <- function(answer_df, query_topics) {
  # Initialize columns in answer_df for each topic with default value of 0
  answer_df[query_topics] <- NA
  
  # Helper function to process each row
  process_row <- function(answer_string) {
    formatted <- format_answer(answer_string, query_topics)
    topics01 <- formatted$topics01
    
    # Return a named list where topic columns are set to 1 or 0 based on topics01
    sapply(query_topics, function(topic) {return(topics01[[topic]])})
  }
  
  # # Apply the processing function to each row in answer_string and bind the result
  library(parallel)
  topic_values <- t(mclapply(answer_df$answer_string, process_row, mc.cores = detectCores() - 2))
  # Bind the result to the original answer_df
  answer_df[query_topics] <- do.call(rbind, topic_values)
  
  
  return(answer_df)
}


