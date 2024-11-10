
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
import pandas as pd
import numpy as np
from joblib import Parallel, delayed
from tqdm import tqdm
import re
import os


os.environ["TOKENIZERS_PARALLELISM"] = "false"
os.environ['PYTORCH_CUDA_ALLOC_CONF'] = 'expandable_segments:True'
import torch
torch.cuda.empty_cache()
print(torch.cuda.is_available())


# Load the pre-trained Sentence Transformer model
# mdl_name = 'stella_en_1.5B_v5' # oom
mdl_name = 'stella_en_400M_v5'
model = SentenceTransformer("dunzhang/"+mdl_name, trust_remote_code=True).cuda()


# Function to generate embeddings
def get_embeddings(text):
    # Generate embeddings
    embedding = model.encode(text)
    return embedding


folder = "gpt_data"
res_df = pd.read_csv("../../"+folder+"/results.csv")
res_df = res_df.fillna("")
fea_df = pd.read_csv("../../data/fea_df.csv")

fea_df['description'] = fea_df['description'].apply(
    lambda x: re.sub(r"Must related to [^(]*\(?.*?\)?\.", "", x).strip()
)



def process_batch(res_df_batch, fea_df, folder, mdl_name, model):
    res_df_with_cos_batch = res_df_batch.copy()
    sm_id_counter = 0  # Counter to track the number of processed sm_id
    
    for sm_id in tqdm(res_df_batch.sm_id.unique(), desc="Processing sm_id"):
        for topic in fea_df.fea.unique():
            refer_str = fea_df.loc[fea_df.fea == topic, 'description'].tolist()[0]
            returned_str = res_df_batch.loc[res_df_batch.sm_id == sm_id, f"{topic}_phrases"].tolist()[0]
            
            if returned_str:
                returned_str_ls = returned_str.split(";")
                query_prompt_name = "s2p_query"
                query_embeddings = model.encode([refer_str], prompt_name=query_prompt_name)
                doc_embeddings = model.encode(returned_str_ls)
                cos_scores = cosine_similarity(query_embeddings, doc_embeddings)[0]
                res_str = "; ".join([f"{returned_str_ls[i]} ({cos_scores[i]})" for i in range(len(returned_str_ls))]) + ";"
                res_df_with_cos_batch.loc[res_df_with_cos_batch.sm_id == sm_id, f"{topic}_phrases"] = res_str
        
        # Increment the counter and check if 1000 sm_id have been processed
        sm_id_counter += 1
        if sm_id_counter % 1000 == 0:
            # Write intermediate results to CSV
            output_path = os.path.join("../../", folder, f"results_with_cos_{mdl_name}.csv")
            res_df_with_cos_batch.to_csv(output_path, index=False)
            print(f"Intermediate results written to {output_path} at {sm_id_counter} sm_id")
    
    # Final write to ensure all data is saved at the end
    output_path = os.path.join("../../", folder, f"results_with_cos_{mdl_name}.csv")
    res_df_with_cos_batch.to_csv(output_path, index=False)
    print(f"Final results written to {output_path}")

    #return res_df_with_cos_batch



process_batch(res_df, fea_df, folder, mdl_name, model)