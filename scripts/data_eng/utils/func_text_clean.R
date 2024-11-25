clean_text <- function(text){
  text <- tolower(text)
  text <- expand_contractions(text)
  return(text)
}

expand_contractions <- function(text){
  # stopwords("english")[grepl("\'",stopwords("english"))]
  # Define a vector of contracted words
  contractions <- c("i'm", "you're", "he's", "she's", "it's", "we're", "they're",
                    "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd",
                    "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll",
                    "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't",
                    "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't",
                    "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't",
                    "mustn't", "let's", "that's", "who's", "what's", "here's",
                    "there's", "when's", "where's", "why's", "how's")
  
  # Define their expansions
  expansions <- c("i am", "you are", "he is", "she is", "it is", "we are", "they are",
                  "i have", "you have", "we have", "they have", "i would", "you would", "he would",
                  "she would", "we would", "they would", "i will", "you will", "he will", "she will",
                  "we will", "they will", "is not", "are not", "was not", "were not",
                  "has not", "have not", "had not", "does not", "do not", "did not",
                  "will not", "would not", "shall not", "should not", "can not", "can not", "could not",
                  "must not", "let us", "that is", "who is", "what is", "here is",
                  "there is", "when is", "where is", "why is", "how is")
  # Create a named vector for easy lookup
  contraction_map <- setNames(expansions, contractions)
  
  for (i in seq_along(contraction_map)) {
    pattern <- paste0("\\b", names(contraction_map)[i], "\\b")
    text <- gsub(pattern, contraction_map[i], text, ignore.case = TRUE)
  }
  
  return(text)
  
}

