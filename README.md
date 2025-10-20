
This repo contains code for the following paper
- Palmer, A. Y., Masuda, Y. J., Harou, A. P., Greene, M. E., Das, J. K., Kwauk, C. T., Robinson, B. E., Koski, A., & Baumgartner, J. (under review). *Impact of tropical cyclones on child marriage rates in 14 affected countries*.

## Code organisation
- `cyclone-marriage.Rproj` is the project file
- `_run.R` is the main script that runs the entire analysis. *This is the only script that needs to be run by the user*.
- The directory `R` contains the functions that support the `_run.R` file
- The directory `data` contains the DHS survey files, TC data, and other reference data
- `utils` contains a short script to help unzip the DHS files

## Main outputs
- The directory `figures` contains all the figures included in the paper
- The directory `results` contains all the results in xlsx files

## Instructions for running the code
DHS files and TC files are not uploaded, and must be directly downloaded from the source.
1. To download DHS files, access must be requested at https://dhsprogram.com/. Datasets should be downloaded via the download manager. 
In order for the code to run, individual recode datasets for all DHS countries must be downloaded in Stata file format and saved under the directory `data/dhs/`. 
The script `utils/unzip_files.R` may be useful in bulk unzipping all files. DHS GPS data should similarly be downloaded and saved in the directory `data/geocodes/`.
The file `data/files.csv` should give you an idea of what the file pathways should look like.
2. The TC data can be downloaded from https://doi.org/10.5880/pik.2017.008. All csv files should be saved in the directory `data/TC_data/`.
3. Once these steps have been completed, run `_run.R` to reproduce the analysis.

Please direct questions/comments/issues to anna.palmer@mail.mcgill.ca.
