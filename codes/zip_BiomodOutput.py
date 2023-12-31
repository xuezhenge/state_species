import os
import zipfile
import shutil
from concurrent.futures import ThreadPoolExecutor

cores_to_use = 4

def zip_and_delete_folder(folder_path):
    """Zips the contents of the given folder and deletes the folder."""
    zip_path = f"{folder_path}.zip"
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                zipf.write(os.path.join(root, file),
                           os.path.relpath(os.path.join(root, file),
                                           os.path.join(folder_path, '..')))
    # Delete folder after successful zipping
    # shutil.rmtree(folder_path)

def main():
    BiomodOuput = '../BiomodOutput'
    folders_to_zip = os.listdir(BiomodOuput)  # Add your folders here

    with ThreadPoolExecutor(max_workers=cores_to_use) as executor:
        executor.map(zip_and_delete_folder, folders_to_zip)

if __name__ == "__main__":
    main()
