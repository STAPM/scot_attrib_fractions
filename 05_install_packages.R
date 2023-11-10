
# The aim of this code is to install the required packages
# with the version specified

# Package names
packages <- c("data.table",
              "ggplot2",
              "cowplot",
              "readxl",
              "knitr",
              "stringr",
              "here",
              "magrittr",
              "RColorBrewer",
              "git2r",
              "getPass",
              "devtools",
              "flextable",
              "bookdown",
              "viridis",
              "rmarkdown",
              "TTR",
              "boot",
              "VGAM",
              "readr",
              "writexl",
              "Rfast",
              "dvmisc",
              "fastmatch",
              "dplyr",
              "plyr",
              "openxlsx",
              "mice",
              "Hmisc",
              "nnet",
              "ggthemes",
              "shiny",
              "DT",
              "shinythemes",
              "ragg")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  #install.packages(packages[!installed_packages], type = "source", INSTALL_opts = "--byte-compile")
  install.packages(packages[!installed_packages], lib = project_lib)
  #install.packages(packages[!installed_packages])
}

###########################
# STAPM packages

# Your gitlab username
uname <- ""


devtools::install_git(
  "https://github.com/stapm/hseclean.git",
  ref = "1.11.3",
  build_vignettes = FALSE, lib = project_lib, quiet = TRUE)

# 
# devtools::install_git(
#   "https://gitlab.com/stapm/r-packages/tobalcepi.git",
#   credentials = git2r::cred_user_pass(uname, getPass::getPass()),
#   ref = "1.6.5",
#   build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/tobalcepi.git",
  build_vignettes = FALSE, quiet = TRUE)



devtools::install_git(
  "https://gitlab.com/stapm/r-packages/stapmr.git",
  credentials = git2r::cred_user_pass(uname, getPass::getPass()),
  ref = "1.9.3",
  build_vignettes = FALSE, quiet = TRUE)

