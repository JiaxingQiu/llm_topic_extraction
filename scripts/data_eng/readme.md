# run scripts in sequence:
- prepare_data.R   prepare raw data for llm to label
- prepare_labeled_df.R   after collecting results from gpt or llama, prepare a labeled df with related phrases
- prepare_human_data.R   after collecting results from gpt4o, prepare the 1080 sampled posts for human evaluation.
- ../embed/sembedding.ipynb caculate embedding distance using a pretrained model
- format_scores.R format distance scores into a dataframe object saved locally as RDS
- adjust_score.R calculate an adjusted score per topic for each post.
- adjust_label.R calculate an adjusted label per topic for each post.