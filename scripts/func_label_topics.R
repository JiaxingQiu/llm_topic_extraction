library(dplyr)
library(stringr)

# Function to format the answer string
format_answer <- function(answer_string) {
  # Regular expression pattern
  pattern <- "\\((\\d+)\\)\\s*([\\w\\s]+):\\s*(yes|no)(?:,\\s*related phrases if any:\\s*'([^']*(?:',\\s*'[^']*)*)')?"
  
  # Extract data using str_match_all
  matches <- str_match_all(answer_string, pattern)[[1]]
  
  # Initialize empty lists to store the results
  topics_detail <- list()
  topics01 <- list()
  
  for (i in seq_len(nrow(matches))) {
    index <- matches[i, 2]
    topic <- str_trim(matches[i, 3])
    if (topic == "body") {
      topic <- "bodyhate"
    }
    label <- str_trim(matches[i, 4])
    phrases <- if (!is.na(matches[i, 5])) str_split(matches[i, 5], "', '")[[1]] else character(0)
    phrases <- str_replace_all(phrases, "^'|'$", "")  # Clean quotes
    
    # Append details to topics_detail list
    topics_detail[[i]] <- list(
      Index = index,
      Topic = topic,
      Label = label,
      `Related Phrases` = phrases
    )
    
    # Set topics01 values to 1 or 0 based on label
    topics01[[topic]] <- ifelse(label == "yes", 1, 0)
  }
  
  return(list(topics_detail = topics_detail, topics01 = topics01))
}


# Function to format the answer dataframe using apply
format_answer_df <- function(answer_df, query_topics) {
  # Initialize columns in answer_df for each topic with default value of 0
  answer_df[query_topics] <- 0
  
  # Helper function to process each row
  process_row <- function(answer_string) {
    formatted <- format_answer(answer_string)
    topics01 <- formatted$topics01
    
    # Return a named list where topic columns are set to 1 or 0 based on topics01
    sapply(query_topics, function(topic) {
      if (topic %in% names(topics01)) {
        return(topics01[[topic]])
      } else {
        return(0)
      }
    })
  }
  
  # # Apply the processing function to each row in answer_string and bind the result
  # topic_values <- t(lapply(answer_df$answer_string, process_row))
  # answer_df[query_topics] <- do.call(rbind, topic_values)
  library(parallel)
  topic_values <- t(mclapply(answer_df$answer_string, process_row, mc.cores = detectCores() - 2))
  # Bind the result to the original answer_df
  answer_df[query_topics] <- do.call(rbind, topic_values)
  
  
  return(answer_df)
}


