
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline

# Clear GPU cache
if torch.cuda.is_available():
    torch.cuda.empty_cache()

model_name = "meta-llama/Llama-3.1-8B-Instruct"  # Replace with exact path for 8B model
tokenizer = AutoTokenizer.from_pretrained(model_name)

print(torch.cuda.is_available())

# Check if CUDA is available
device = 0 if torch.cuda.is_available() else -1  # Use GPU if available, otherwise fallback to CPU

# Load the model with the appropriate device settings
model = AutoModelForCausalLM.from_pretrained(
    model_name,                   
    return_dict=True,
    low_cpu_mem_usage=True,
    torch_dtype=torch.float16,     # Use float16 for efficient memory usage on GPU
    device_map="auto" if device == 0 else None,  # Automatically distribute across devices or use CPU
    trust_remote_code=True
)

# Initialize the text generation pipeline
pipe = pipeline(
    "text-generation",
    model=model,
    tokenizer=tokenizer,
    torch_dtype=torch.float16,
    device_map="auto" if device == 0 else None,  # Explicitly set device to GPU if available, else CPU
)
# For model_0
for name, param in model.named_parameters():
    print(f"{name} is on {param.device}")


import pandas as pd
import numpy as np
answer_df = pd.read_csv("./llama_data/answer_df_new.csv")
query_df = pd.read_csv("./llama_data/query_df3.csv")
print(answer_df.shape)


from time import time


system_message = """
You are an AI assistant designed to accurately answer questions.
Restrict your answer to the exact question and use the exact answer format asked.
If yes, always provide related phrases. 
Answer should not have implied information.
"""


def colorize_text(text):
    for word, color in zip(["Reasoning", "Question", "Answer", "Total time"], ["blue", "red", "green", "magenta"]):
        text = text.replace(f"{word}:", f"\n\n**<font color='{color}'>{word}:</font>**")
    return text

def format_prompt(text):
    question_content = "Does the paragraph mention any of the following topics:\n"
    for i in range(len(query_df)):
        question_content += f"  ({i+1}) {query_df.topic[i]}: {query_df.description[i]}.\n"
    answer_content = "Return answer in format:\n"
    for i in range(len(query_df)): 
        answer_content += f"  ({i+1}) {query_df.topic[i]}: [yes/no], related phrases if any: \n"
    paragragh_content = f"Paragraph: '{text}' \n"
    user_message = question_content + answer_content + paragragh_content
    #print(user_message)
    
    return user_message



def query_model_batch(
        system_message,
        user_messages,
        temperature=0,
        max_length=1024
    ):
    start_time = time()
    # Add "Question: ... Answer:" to each user message for clarity
    batched_messages = [
        "Question: " + message + " Answer:" for message in user_messages
    ]
    
    # Construct prompts for each message in batch
    all_prompts = [
        pipe.tokenizer.apply_chat_template(
            [
                {"role": "system", "content": system_message},
                {"role": "user", "content": user_message}
            ],
            tokenize=False,
            add_generation_prompt=True
        ) for user_message in batched_messages
    ]
    
    # Define the end-of-sequence terminators
    terminators = [
        pipe.tokenizer.eos_token_id,
        pipe.tokenizer.convert_tokens_to_ids("<|eot_id|>")
    ]
    
    # Run the batch inference
    sequences = pipe(
        all_prompts,
        do_sample=True,
        top_p=0.5,
        temperature=temperature,
        num_return_sequences=1,
        eos_token_id=terminators,
        max_new_tokens=max_length,
        return_full_text=False,
        pad_token_id=terminators[0]
    )
    
    # Extract generated text for each sequence
    answers = []
    for i, sequence in enumerate(sequences):
        answer = sequence[0]['generated_text']
        total_time = f"Total time: {round(time() - start_time, 2)} sec."
        # Format the response with timing information
        answers.append(batched_messages[i] + " " + answer + " " + total_time)

    return answers






for k in range(1000000):
    batch_size = 10

    # Filter for rows where 'answer_string' is NaN
    unanswered_df = answer_df[answer_df['answer_string3'].isna()]

    # Get the indices of these NaN entries in the original DataFrame
    indices_to_update = unanswered_df.index[:batch_size]

    # Prepare prompt content for the first 10 entries with NaN answer_string
    prompts = [format_prompt(text) for text in unanswered_df['text_w_eos'].iloc[:batch_size]]

    # Save the indices list if needed for later use
    indices_to_update_list = list(indices_to_update)


    # Batch process all prompts at once
    responses = query_model_batch(
        system_message=system_message,
        user_messages=prompts,
        temperature=0.1,
        max_length=512
    )


    # Display and process responses in a loop
    for i, response in enumerate(responses):
        #display(Markdown(colorize_text(f"{response}")))

        # Extract answer if available
        if "Answer:" in response:
            answer = response.split("Answer:")[1]
            # Use the original index from indices_to_update_list
            answer_df.loc[indices_to_update_list[i], 'answer_string3'] = answer
        else:
            # Use the original index from indices_to_update_list
            answer_df.loc[indices_to_update_list[i], 'answer_string3'] = "Answer not found"


    print(answer_df.loc[indices_to_update_list, :])
    answer_df.to_csv('./llama_data/answer_df_new.csv', index=False)

