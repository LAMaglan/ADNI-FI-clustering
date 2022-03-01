rm(list = ls(all = TRUE))

# edit to your filepath containing "list_frailty_indices.csv" and "list_final_variables.csv"
# (i.e. where you place cloned git repo)
wd <- "" 


out_dir <- paste0(wd, "/intermediate_files/")
dir.create(file.path(wd, "intermediate_files"), showWarnings = FALSE)
results_dir <- paste0(wd, "/script_results")
dir.create(file.path(wd, "script_results"), showWarnings = FALSE)

setwd(wd)


#cross-validation params

# TEST
nfolds = 3
nrep = 3

# nfolds = 10
# nrep = 100

# cumulative % variance threshold for choosing number of comps
# (1-100)
FAMD_threshold <- 80

# TRUE if filtering AD in one_percent rule
#filter_one_percent = TRUE
filter_one_percent = FALSE


# Valid code for loop is "ADNI1" and/or "ADNI2 & ADNIGO"
ADNI_loop_sample <- c("ADNI1", "ADNI2 & ADNIGO")



# name of the frailty index in list_frailty_indices.csv
# that you want as your "standard" frailty index, which
# will be used as the basis for refinement (using FAMD and clustering)
# (note: specifically for "FI_expert", write "FI_Expert")
chosen_FI_standard <- "FI_Expert"



# Define whether to include covariates in analysis comparing clusters
# (for refinement of frailty index)
# include a vector with any of the following: 
# FI_refined_age_sex_edu, FI_refined_age_sex, and/or FI_refined_no_cov
# FI_refined_age_sex_edu: include age, sex, and education as covariates
# FI_refined_age_sex: include age and sex as covariates
# FI_refined_no_cov: do not include any covariates
# for the final analyses, FI_refined_no_cov was chosen
FI_refined_list <- c("FI_refined_no_cov")



## Additional paramaters


# Set to TRUE if you want to include
# FI from Canevelli
include_Canevelli = TRUE
# include_Canevelli = FALSE

# Set to TURE if you want to recompute all FIs
# (regardless of value, script will recompute
# any new FIs)
# recompute_FI_refined <- TRUE
 recompute_FI_refined = FALSE


save_cluster_results = TRUE
#save_cluster_results = FALSE

save_ML_table = TRUE
#save_ML_table = FALSE

save_cox_table =TRUE
#save_cox_table = FALSE



# number of FAMD components to be included in FAMD plots
num_PCs_plot <- 5

for (ADNI_sample in ADNI_loop_sample) {
  
  print(paste0("---------------starting analyses for ", ADNI_sample, "--------------"))
  
  # reset increment when working on "new" ADNI_sample
  increment = 0
  only_for_ADNI1_Canevelli = FALSE
  only_for_ADNI2_Canevelli = FALSE
  
  for (chosen_FI_refined in FI_refined_list){
    
      print(paste0("------ refined FI is: ", chosen_FI_refined, "----"))
      recompute_FI_refined_loop = recompute_FI_refined #reseting here
    
      if (chosen_FI_refined == "FI_expert_MCI_clustering_FDR_005"){
        analysis_subtitle = "(refined FI is corrected for age, sex, and education)"
        FI_explained = "refined_FI_age_sex_edu_corrected"
        ML_name <-"Age_sex_edu_corr"
      } else if (chosen_FI_refined == "FI_refined_age_sex"){
        analysis_subtitle = "(refined FI is corrected for age and sex)"
        FI_explained = "refined_FI_age_sex_corrected"
        ML_name <-"Age_sex_corr"
      } else if (chosen_FI_refined == "FI_refined_no_cov"){
        analysis_subtitle = "(refined FI is unadjusted for covariates)"
        FI_explained = "refined_FI_no_adjustment"
        ML_name <-"no_cov"
      }
      
      if (ADNI_sample=="ADNI1"){
        prefix = "ADNI1"
        prep_title= "ADNI1"
        analysis_title = "Clustering refinement (FAMD + HCPC) of FI and analyses"
        analysis_suffix = paste0("clustering_", FI_explained)
        
        if (include_Canevelli) only_for_ADNI1_Canevelli = TRUE
        
      } else if (ADNI_sample == "ADNI2 & ADNIGO"){
        prefix = "ADNI2_ADNIGO"
        prep_title = "ADNI2 and ADNIGO"
        analysis_title = "Validation analyses using ADNI2 and ADNIGO combined"
        analysis_suffix = paste0("validation_", FI_explained)
        
        if (include_Canevelli) only_for_ADNI2_Canevelli = TRUE
        
      } else if (ADNI_sample == "ADNI2"){
        prefix = "ADNI2"
        prep_title = "ADNI2"
        analysis_title = "Validation analyses using ADNI2"
        analysis_suffix = paste0("validation_", FI_explained)
      } else if (ADNI_sample == "ADNIGO"){
        prefix = "ADNIGO"
        prep_title = "ADNIGO"
        analysis_title = "Validation analyses using ADNI2"
        analysis_suffix = paste0("validation_", FI_explained)
      }
      
      increment = increment + 1
      
      
      if (increment == 1){
        rmarkdown::render('scripts/compute_FI.Rmd', 
                          output_file = paste0(prefix, "_FI_main.html"),
                          output_dir = paste0(wd),
                          quiet = TRUE)

        rm(list=setdiff(ls(), c("ADNI_sample", "prefix", "prep_title", "analysis_suffix",
                                "wd", "out_dir", "results_dir",
                                "analysis_title", "analysis_subtitle",
                                "chosen_FI_standard", "chosen_FI_refined",
                                "nfolds", "nrep", "ML_name",
                                "recompute_FI_refined_loop", "recompute_FI_refined",
                                "FI_refined_list", "num_PCs_plot",
                                "save_cluster_results", "save_ML_table",
                                "save_cox_table", "increment",
                                "include_Canevelli", "only_for_ADNI1_Canevelli",
                                "only_for_ADNI2_Canevelli", "FAMD_threshold",
                                "filter_one_percent")))

      }
      
      rmarkdown::render('scripts/refine_FI_analyses.Rmd', 
                        output_file = paste0(prefix, "_FI_", analysis_suffix, "_analyses.html"),
                        output_dir = paste0(wd),
                        quiet = TRUE)
      
      if ( (add_new_FI_to_list | recompute_FI_refined_loop) & ADNI_sample=="ADNI1" & increment > 0){


        # repeat preprocessing script to compute FI for "new" FI
        if (add_new_FI_to_list){
          print("Computing new FI")
        } else {
          print("Recomputing FI")
        }


        rm(list=setdiff(ls(), c("analysis_title","nfolds", "nrep",
                                "wd", "out_dir", "results_dir",
                                "analysis_subtitle", "analysis_suffix",
                                "prep_title", "prefix", "ADNI_sample",
                                "chosen_FI_standard", "chosen_FI_refined",
                                "recompute_FI_refined", "recompute_FI_refined_loop",
                                "FI_refined_list", "num_PCs_plot",
                                "save_cluster_results", "save_ML_table",
                                "save_cox_table", "increment",
                                "include_Canevelli", "only_for_ADNI1_Canevelli",
                                "only_for_ADNI2_Canevelli", "FAMD_threshold",
                                "filter_one_percent")))

        #otherwise it will skip cox, ML, etc.
        add_new_FI_to_list = FALSE
        recompute_FI_refined_loop = FALSE

        rmarkdown::render('scripts/compute_FI.Rmd', 
                          output_file = paste0(prefix, "_FI_main.html"),
                          output_dir = paste0(wd),
                          quiet = TRUE)

        rm(list=setdiff(ls(), c("ADNI_sample", "prefix", "prep_title", "analysis_suffix",
                                "analysis_title", "analysis_subtitle",
                                "wd", "out_dir", "results_dir",
                                "chosen_FI_standard", "chosen_FI_refined",
                                "nfolds", "nrep", "ML_name",
                                "recompute_FI_refined", "recompute_FI_refined_loop",
                                "FI_refined_list", "num_PCs_plot",
                                "save_cluster_results", "save_ML_table",
                                "save_cox_table", "increment",
                                "include_Canevelli", "only_for_ADNI1_Canevelli",
                                "only_for_ADNI2_Canevelli", "FAMD_threshold",
                                "filter_one_percent")))


        # note: only set up for ADNI1 and ADNI2/ADNIGO (not ADNI2 or ADNIGO by)
        print("rerunning clustering script")
        rmarkdown::render('scripts/refine_FI_analyses.Rmd', 
                          output_file = paste0(prefix, "_FI_", analysis_suffix, "_analyses.html"),
                          output_dir = paste0(wd),
                          quiet = TRUE)

      }

      
      rm(list=setdiff(ls(), c("prep_title", "prefix", "analysis_suffix",
                              "ADNI_sample", "chosen_FI_standard", "chosen_FI_refined",
                              "wd", "out_dir", "results_dir",
                              "recompute_FI_refined", "recompute_FI_refined_loop",
                              "nfolds", "nrep",
                              "FI_refined_list", "num_PCs_plot",
                              "save_cluster_results", "save_ML_table",
                              "save_cox_table", "increment",
                              "include_Canevelli", "only_for_ADNI1_Canevelli", 
                              "only_for_ADNI2_Canevelli", "FAMD_threshold",
                              "filter_one_percent")))
      
      
    }
    
  print(paste0("---------Finished with ", ADNI_sample, "---------------"))
  
}
  
  


