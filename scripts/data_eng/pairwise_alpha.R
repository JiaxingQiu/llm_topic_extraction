setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
# install.packages("irr")
library(irr)


# calculate pairwise Krippendorff's Alpha coefficients among several coders on a specific topic 

# ---- data 2 ----
source("./scripts/data_eng/prepare_analysis2.R")
rm_ls <- list()
for(topic in c("stigma")){
  data <- data.frame("llama" = label_df_llama[[topic]],
                     "mistral" = label_df_mistral[[topic]],
                     "qwen" = label_df_qwen[[topic]],
                     "vicuna" = label_df_vicuna7b[[topic]])
  data <- as.matrix(data)
  # get pairwise alpha in a matrix (upper tri only)
  res_mat <- matrix(nrow=ncol(data), ncol=ncol(data))
  colnames(res_mat) <- colnames(data)
  rownames(res_mat) <- colnames(data)
  for(i in 1:(ncol(data)-1)) {
    for(j in (i+1):ncol(data)){
      res_mat[colnames(data)[i], colnames(data)[j]] <-  kripp.alpha(data[,c(i,j)], method="nominal")$value
      res_mat[colnames(data)[j], colnames(data)[i]] <- res_mat[colnames(data)[i], colnames(data)[j]]
    }
  }
  # Add the result matrix to the list with the topic name as the key
  rm_ls[[topic]] <- res_mat
  
}

# Print results for inspection
print(rm_ls)



#  Generate heatmap using pheatmap
pheatmap(as.matrix(res_mat), 
         main = "Pairwise Krippendorff's Alpha", 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean",
         clustering_method = "complete",
         color = colorRampPalette(c("blue", "white", "red"))(100),  # Blue to red color palette
         display_numbers = TRUE) 

# Assuming `res_mat` is the result matrix for the topic "stigma"
threshold <- 0.005
disagreement_count <- rep(0, ncol(res_mat))
average_alpha <- rowMeans(res_mat, na.rm = TRUE)  # calculates the average ignoring NA values

for(i in 1:ncol(res_mat)) {
  for(j in 1:ncol(res_mat)) {
    if(i != j && res_mat[i, j] < threshold) {
      disagreement_count[i] <- disagreement_count[i] + 1
    }
  }
}
# Combine the results into a data frame
coder_stats <- data.frame(coder = colnames(res_mat), average_alpha, disagreement_count)

# Sort by disagreement count and average_alpha to see which coders disagree most
coder_stats <- coder_stats[order(coder_stats$disagreement_count, decreasing = TRUE), ]

# Print coder stats
print(coder_stats)
