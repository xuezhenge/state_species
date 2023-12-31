import os
import shutil

# Define the path to the parent directory
parent_dir = "../outputs"

# Iterate through all items in the parent directory
for item in os.listdir(parent_dir):
    item_path = os.path.join(parent_dir, item)
    # Check if the item is a directory
    if os.path.isdir(item_path):
        # Iterate through all sub-items in this directory
            filename = os.path.join(item_path, 'data_cleaning/raw_data_global.csv')
            # If the sub-item is a directory, remove it
            if not os.path.isfile(filename):
                print(f"{filename} not exists")
