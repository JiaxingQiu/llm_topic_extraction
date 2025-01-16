setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
library(dplyr)
library(kableExtra)
library(knitr)
fea_df <- read.csv("./data2/fea_df.csv")
fea_df$description <- sub("\\.$", "", fea_df$description)
fea_df$description <- paste0(fea_df$description, ".")

fea_df[,c("description")] %>% 
  kable(caption="Case study 2 -- Topic Descriptions",
        format = "latex", 
        col.names = c("Subcategories of Weight Stigma in Treatment"),
        booktabs = T,
        align="l") %>% 
  column_spec(column = 1, width = "5.5in")%>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro") 

