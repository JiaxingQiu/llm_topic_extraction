# data

### inputs
- "./data/answer_df_raw.csv" contains 77175 social media posts.
- "./data/fea_df.csv" contains topical features to be queried by LLM. 

### outputs
- create subfolder "llama_data" and "gpt_data" locally.
- llama extracted topics should be stored as "./llama_data/answer_df.csv"
- GPT4 extracted topics should be stored as "./gpt_data/answer_df.csv"


# script
- llama_inference_3.1.ipynb and llama_inference.py are main scripts generating topic labels.
- demo_gpt_inference.ipynb is the pseudo code for GPT4 qeuries, containing necessary function (e.g. format_prompt)