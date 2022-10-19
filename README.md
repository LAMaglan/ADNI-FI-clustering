# ADNI-FI-clustering

Study using code published in [GeroScience](https://doi.org/10.1007/s11357-022-00669-2). See citation below if using code.
<br>
<br>
Code and files to compute, refine and run analyses for frailty indices
<br>
based on data from the Alzheimer's Disease Neuroimaging Iniative (ADNI):
<br>
http://adni.loni.usc.edu/
<br>
To obtain access to data from ADNI, see this link:
<br>
http://adni.loni.usc.edu/data-samples/access-data/
<br>
<br>
First,follow details of downloading download the ADNIMERGE R-package (ADNIMERGE_0.0.1.tar.gz) from the following link:
<br>
https://adni.bitbucket.io/
<br>
Second, install required packages by running `install_packages.R`.
<br>
Finally, to run all analyses, start the code in `run_scripts.R`.
<br>
This will run both `compute_FI.Rmd` and `refine_FI_analyse.Rmd`.
<br>
See below for some details of the scripts (and more in the actual scripts).
<br>
Briefly, ADNI1 is used as the basis for refinement of any frailty index and the
<br>
main analyses, while ADNI2 combined with ADNIGO is used for validation.
<br>
These scripts have been run with R version 3.6.2 on Windows 11.

## Details of scripts
<br>
Note: HTML-reports (one for each Rmarkdown file and "sample") will be generated in the working 
<br>
directory (i.e. repo location). All other files will be outputed to subfolders `script_results/`
<br>
and `intermediate_files/` in current working directory (both modifiable to some extent)
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
HTML-report (in the working directory) that can be inspected for some summary reports.
<br>
<br>
**refine_FI_analyses.R**
<br>
Refines your chosen frailty index using dimensionality reduction (FAMD) and clustering (HCPC) for ADNI1.
<br>
Note that this will add a column with newly refined frailty index to *list_frailty_indices.csv*
<br>
Then, runs a set of analyses including binary classification and cox-regression (survival analyses) for
<br>
both ADNI1 (main) as well as ADNI2 combined with ADNIGO (validation). An HTML-file is generated (in the 
<br>
working directory) containing detailed reports (and figures) of various steps and analyses
<br>
<br>
**run_scripts.R**
<br>
Has modifiable paramaters that affect mainly `refine_FI_analyses.Rmd`, including which sample to run the 
<br>
data with (i.e. ADNI1 and/or ADNI2 & ADNIGO), and what output to save. For the first run, ADNI1 must be used 
<br>
to refine the FI, and build machine learning models that will be used in the validation analyes (ADNI2/ADNIGO). 
<br>
There is an internal logic of when  to rerun the two Rmarkdown (.Rmd) scripts.
<br>
<br>
**additional_figures.R**
<br>
Code to generate some of the figures. Need to add path to working directory (i.e. where the github
<br>
repo is cloned to). Takes an input csv-files from `refine_FI_analyses.Rmd`

**mortality.R**
<br>
Code to run mortality analyses. Need to add path to working directory  (i.e. where csv-files from
<br>
`refine_FI_analyses.Rmd` are) and add name of said csv-file

## Citation
```
Engvig, A., Maglanoc, L.A., Doan, N. et al. Data-driven health deficit assessment improves a frailty 
index’s prediction of current cognitive status and future conversion to dementia: results from ADNI. 
GeroScience (2022). https://doi.org/10.1007/s11357-022-00669-2
```
