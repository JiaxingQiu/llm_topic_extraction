import json

# split a jsonl file to n jsonl files
def split_file(input_file, num_files):
    with open(input_file, "r") as f:
        lines = f.readlines()
    num_lines = len(lines)
    num_lines_per_file = num_lines // num_files
    for i in range(num_files):
        if i == num_files - 1:
            with open(f"{input_file[:-6]}_{i+1}.jsonl", "w") as f:
                f.writelines(lines[i*num_lines_per_file:])
        else:
            with open(f"{input_file[:-6]}_{i+1}.jsonl", "w") as f:
                f.writelines(lines[i*num_lines_per_file:(i+1)*num_lines_per_file])

def fist_k_lines(input_file, k):
    with open(input_file, "r") as f:
        lines = f.readlines()
    with open(f"{input_file[:-6]}_{k}.jsonl", "w") as f:
        f.writelines(lines[:k])

if __name__ == "__main__":
    input_file = "batch_request_gpt4omini.jsonl"
    num_files = 80

    split_file(input_file, num_files)
    # fist_k_lines(input_file, 1000)