# run scripts in sequence:
- prepare_data.R   prepare raw data for llm to label
- prepare_labeled_df.R   after collecting results from gpt or llama, prepare a labeled df with related phrases
- ../embed/sembedding.ipynb caculate embedding distance using a pretrained model
- format_scores.R format distance scores into a dataframe object saved locally as RDS
- adjust_label.R calculate an adjusted label score per topic for each post.