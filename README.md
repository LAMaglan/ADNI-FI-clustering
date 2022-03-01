# ADNI-FI-clustering

Code and files to compute, refine and run analyses for frailty indices
<br>
based on data from the Alzheimer's Disease Neuroimaging Iniative (ADNI):
<br>
http://adni.loni.usc.edu/
<br>
<br>
First, download the ADNIMERGE R-package (ADNIMERGE_0.0.1.tar.gz) from the following link:
<br>
https://adni.bitbucket.io/
<br>
Second, install required packages by running `install_packages.R`.
<br>
Finally, to run all analyses, start the code in `run_scripts.R`.
<br>
This will run both `compute_FI.Rmd` and `refine_FI_analyse.Rmd`.
<br>
See below for some details of the scripts (and more in the actual scripts)
<br>
<br>
## Details of scripts
<br>
Note: HTML-reports will be generated in the working directory (i.e. repo location).
<br>
All other files will be outputed to subfolders `script_results/` and
<br>
`intermediate_files/` in current working directory (both modifiable to some extent)
<br>
<br>

**install_packages.R**
<br>
Installs the required packages for both `compute_FI.Rmd` and `refine_FI_analyses.Rmd`.
<br>
Note: packages in `additional_figures.R` will have to be installed manually.
<br>
<br>
**compute_FI.Rmd**
<br>
Computes frailty indicies based on what is defined in both *list_variables_final.csv*
<br>
and *list_frailty_indices.csv*. The former csv-file can be edited to compute a frailty index
<br>
based on different combinations of variables (if so, edit relevant line in `run_scripts.R`).
<br>
This creates a csv-file that is used as input in `refine_FI_analyses.Rmd`, as well as an
<br>
HTML-report (in the workingd directory) that can be inspected for some summary reports.
<br>
<br>
**refine_FI_analyses.R**
<br>
Refines your chosen frailty index using dimensionality reduction (FAMD) and clustering (HCPC).
<br>
Note that this will add a column with newly refined frailty index to *list_frailty_indices.csv*
<br>
Then, runs a set of analyses including binary classification and cox-regression (survival analyses).
<br>
An HTML-file is generated (in the working directory) containing detailed reports (and figures) of 
<br>
various steps and analyses
<br>
<br>
**run_scripts.R**
<br>
Has modifiable paramaters that affect mainly `refine_FI_analyses.Rmd`, including
<br>
which sample to run the data with (i.e. ADNI1 and/or ADNI2 & ADNIGO), and what output to save.
<br>
There is an internal logic of when to rerun the two Rmarkdown (.Rmd) scripts.
<br>
<br>
**additional_figures.R**
<br>
Code to generate some of the figures. Need to add path to working directory (i.e. where the github
<br>
repo is cloned to). Takes an input csv-files from `refine_FI_analyses.Rmd`