import os
models = ['GFDL-ESM4','IPSL-CM6A-LR','MPI-ESM1-2-HR','MRI-ESM2-0','UKESM1-0-LL']
rcps = ['ssp126','ssp585']

for model in models:
    for rcp in rcps:
        folder = '../chelsa_climate_data/2071-2100/' + model + '/' + rcp
        count = 1
        # count increase by 1 in each iteration
        # iterate all files from a directory
        for file_name in os.listdir(folder):
            print(file_name)
            #import pdb;pdb.set_trace()
            # Construct old file name
            source = os.path.join(folder,file_name)
            
            # New file name
            new_name = file_name.split("_")[1]
            # Adding the count to the new file name and extension
            destination = os.path.join(folder,f'{new_name}.tif')

            # Renaming the file
            os.rename(source, destination)
            count += 1
        print('All Files Renamed')

        print('New Names are')
        # verify the result
        res = os.listdir(folder)
        print(res)