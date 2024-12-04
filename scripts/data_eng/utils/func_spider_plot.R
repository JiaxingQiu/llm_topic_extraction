spider_plot <- function(label_df, title_str="default", norm=F){
  # Load necessary libraries
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Assuming 'label_df' and 'fea_df' are predefined
  # Select only relevant columns in 'label_df' according to 'fea_df$fea' and a grouping variable 'group'
  label_df <- label_df[, c("group", fea_df$fea)]
  
  # Melt 'label_df' to long format to facilitate aggregation
  long_label_df <- label_df %>%
    pivot_longer(cols = all_of(fea_df$fea), names_to = "feature", values_to = "value")
  
  # Calculate mean of each feature by group
  mean_df <- long_label_df %>%
    group_by(group, feature) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
  if(norm){
    # Normalize the means for each feature within each group
    mean_df <- mean_df %>%
      group_by(feature) %>%
      mutate(mean_value = mean_value / max(mean_df$mean_value)) %>%#  max(mean_value)
      ungroup()
  }else{
    mean_df$mean_value[which(mean_df$mean_value>0.65)] <- 0.65
  }
  mean_df$feature <- factor(mean_df$feature, levels = fea_df$fea)
 
  mean_df <- as.data.frame(mean_df) %>% arrange(feature)
  
  # Create the spider plot using ggplot2 with polar coordinates
  spider_plot <- ggplot(data = mean_df, aes(x = feature, y = mean_value, group = group, fill = group)) +
    geom_polygon(alpha = 0.3) +  # Use polygons to create filled areas for each group
    geom_line(aes(color = group)) +  # Add lines to outline the shapes
    theme_minimal() +  # Use a minimal theme for cleaner visuals
    coord_polar(start = 1, clip = "off") +  # Convert to polar coordinates for the spider plot effect
    labs(title = NULL,
         subtitle = ifelse(grepl("label", tolower(title_str)), "Label", "Score"),
         x = NULL, y = NULL) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5),  
          plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5), # center it
          axis.text.x = element_text(angle = 0, size = 8, face="bold"),  # Adjust feature labels for better readability
          legend.title = element_blank() )
  
  if(grepl("label", tolower(title_str))){
    spider_plot <- spider_plot + 
      ylim(0, 0.65)
      
  }else{
    spider_plot <- spider_plot + 
      ylim(0, 0.35)
      
  }
  
  if(grepl("Human",title_str) | grepl("GPT-4o" , title_str)){
    spider_plot <- spider_plot + 
      theme(panel.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"))
  }
  if(grepl("Human",title_str)){
    spider_plot <- spider_plot + ggtitle("Human")
  }
  if(grepl("Label by GPT-4o", title_str)){
    spider_plot <- spider_plot + ggtitle("GPT-4o")
  }
  # Display the plot
  return(spider_plot)
  
}
