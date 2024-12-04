# run scripts in sequence:

- prepare_data.R   prepare raw data for llm to label
- prepare_labeled_df.R   after collecting results from gpt or llama, prepare a labeled df with related phrases
- prepare_human_data.R   after collecting results from gpt4o, prepare the 1080 sampled posts for human evaluation.
- ../embed/embedding.ipynb calculate embedding distance using a pretrained model
- prepare_scores.R format distance scores into a dataframe object saved locally as RDS
- prepare_analysis.R is the main script to prepare datasets for downstrain analysis


- exclude_outliers.R

- ensemble_score.R calculate an adjusted score per topic for each post.
- ensemble_label.R calculate an adjusted label per topic for each post.
- ensemble_label_sampling_parallel.R
- ensemble_label_sampling_parallel2.R

- res_eng folder engineer paper plots
