# use packrat to manage package versions/dependencies (optional)
install.packages("packrat")

packrat::init(
  infer.dependencies = FALSE,
  enter = TRUE,
  restart = FALSE)

# Get package specific versions (used in current analyses)
# otheriwse, replace "install_version" below (and version numbers)
# with simple call to "install.packages" (from base R)
install.packages("devtools")
library("devtools")

# Generate HTML-files
install_version("rmarkdown", version = "2.10")

# set path to location of ADNIMERGE_0.0.1.tar.gz
# (R-package containing all relevant ADNI data)
install.packages("ADNIMERGE_0.0.1.tar.gz", repos = NULL, type = "source")

### compute FI script ###
install_version("dplyr", version = "1.0.0")
install_version("car", version = "3.0.11") 
install_version("ggplot2", version = "3.3.5")
install_version("reshape2", version = "1.4.4")
install_version("lubridate", version = "1.7.10") 
install_version("stringr", version = "1.4.0")

### cluster analysis script ###
install_version("tidyr", version = "1.1.3")
install_version("maggritr", version = "2.0.1")
install_version("bnstruct", version = "1.0.10") 
install_version("ggsignif", version = "0.6.2")
install_version("FactoMineR", version = "2.4") 
install_version("factoextra", version = "1.0.7") 
install_version("fdm2id", version = "0.9.5")
install_version("cowplot", version = "1.1.1") 
install_version("moonBook", version = "0.2.4") 
install_version("ztable", version = "0.2.2") 
install_version("afex", version = "1.0.1") 

install_version("discrim", version = "0.1.3")
install_version("tidymodels", version = "0.1.3") 


install_version("survival", version = "3.2.12")
install_version("survminer", version = "0.4.9") 
install_version("coxphw", version = "4.0.2") 
install_version("timeROC", version = "0.4") 
install_version("patchwork", version = "1.1.1") 

install_version("rrtable", version = "0.2.1") 


### Take snapshot ###

packrat::snapshot(
  snapshot.sources = FALSE,
  ignore.stale = TRUE,
  infer.dependencies = FALSE)
