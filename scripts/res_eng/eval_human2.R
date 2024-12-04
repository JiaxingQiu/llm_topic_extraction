setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
library(dplyr)
library(readxl)

# table subject and narratives
df <- read_excel("./data2/raw/stigma.xlsx")
n_distinct(df$record_id)
length(df$record_id)


# read in excel 
data <- read_excel("./data2/raw/coders2.xlsx")
data$coder1 <- ifelse(tolower(data$`Weight Stigma? Noelle`)=="yes",1,0)
data$coder2 <- ifelse(tolower(data$`Weight Stigma? Khush`)=="yes",1,0)
data$consensus <- tolower(data$`Consensus [Yes, No, No Consensus]`)
data$coder2[which(data$consensus == "yes")] <- 1
data$coder2[which(data$consensus == "no")] <- 0

label_data <- data.frame("coder1" = as.numeric(data$coder1),
                         "coder2" = as.numeric(data$coder2))
label_data <- label_data[complete.cases(label_data),]
irrCAC::fleiss.kappa.raw(label_data)$est
irrCAC::gwet.ac1.raw(label_data)$est


mean(data$coder1)
