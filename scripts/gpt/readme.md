### batch preparation
- run ```preprocess.py``` to preprocess the csv file to jsonl file that gpt can read. Please change the file you want to process, default is ```data/answer_df_raw.csv```


### Job submission
-  run ```job.ipynb```, and follow the instructions

#### API_KEY
You should create a file named ```key.py``` in this directory. And the content should be ```openai_key = "{your openai api key}"```

**Be sure that the ```key.py``` should not be uploaded to github to avoid leakage.**