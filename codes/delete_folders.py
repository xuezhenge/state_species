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
        subfolders = ['omission_rates','sp_bios_names','DataSplitTable','pre_pa_csv','summary_doc']
        for sub_item in subfolders:
            sub_item_path = os.path.join(item_path, sub_item)
            # If the sub-item is a directory, remove it
            if os.path.isdir(sub_item_path):
                shutil.rmtree(sub_item_path)
                print(f"Removed subfolder: {sub_item_path}")


# import os
# import shutil
# from concurrent.futures import ThreadPoolExecutor

# def delete_folder(folder_path):
#     folder_path = os.path.join(parent_dir,folder_path)
#     if os.path.exists(folder_path) and os.path.isdir(folder_path):
#         shutil.rmtree(folder_path)
#         print(f"Deleted folder: {folder_path}")
#     else:
#         print(f"Folder not found: {folder_path}")

# parent_dir = "../BiomodOutput"
# folders_to_delete = os.listdir(parent_dir)  # Add your folder paths here
# with ThreadPoolExecutor(max_workers=24) as executor:  # Adjust max_workers as needed
#     executor.map(delete_folder, folders_to_delete)
