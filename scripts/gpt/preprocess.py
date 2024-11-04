import pandas as pd
import json

def colorize_text(text):
    for word, color in zip(["Reasoning", "Question", "Answer", "Total time"], ["blue", "red", "green", "magenta"]):
        text = text.replace(f"{word}:", f"\n\n**<font color='{color}'>{word}:</font>**")
    return text

def format_prompt(query_df, text):
    question_content = "Does the paragraph mention any of the following topics:\n"
    for i in range(len(query_df)):
        question_content += f"  ({i+1}) {query_df.fea[i]}: {query_df.description[i]}.\n"
    answer_content = "Return answer in format:\n"
    for i in range(len(query_df)): 
        answer_content += f"  ({i+1}) {query_df.fea[i]}: [yes/no], related phrases if any: \n"
    paragragh_content = f"Paragraph: '{text}' \n"
    user_message = question_content + answer_content + paragragh_content
    #print(user_message)
    
    return user_message

def main():
    # Load CSV file
    df = pd.read_csv("data/answer_df_raw.csv", names=["sm_id", "text_w_eos"])  # Replace "input.csv" with your file path
    query_df = pd.read_csv("data/fea_df.csv")
    df = df.iloc[1:]

    system_message = """
    You are an AI assistant designed to answer questions.
    Please restrict your answer to the exact question and use the exact answer format asked.
    Answer should not have implied information. If the answer is yes, always provide related phrases. 
    """

    # Construct JSON structure
    json_data = []
    for _, row in df.iterrows():
        entry = {
            "custom_id": row["sm_id"],
            "method": "POST",
            "url": "/v1/chat/completions",
            "body": {
                "model": "gpt-4o-mini", # Change this to the model you want to use
                "messages": [
                    {"role": "system", "content": system_message},
                    {"role": "user", "content": format_prompt(query_df, row["text_w_eos"])},
                ],
                "max_tokens": 2000, # Change this to the desired max tokens
                "temperature": 0.1
            }
        }
        json_data.append(entry)
    
    # Save JSON data t
    with open("data/batch_request_gpt4omini.jsonl", "w") as f:
        for data in json_data:
            f.write(json.dumps(data) + "\n")
    print("JSON file created as 'data/batch_request_gpt4omini.jsonl'")


if __name__ == "__main__":
    main()