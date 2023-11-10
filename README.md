# Alcohol and tobacco attributable fractions for Scotland

## Purpose of this code
This code was used in a project to calculate the Scottish alcohol, smoking, and joint alcohol & smoking attributable fractions, using Scottish Health Survey (SHES) data.   

The code used to conduct the calculations is made open source for the following two reasons:

- Transparency. Open science, allowing review and feedback to the project team on the code and methods used.  

- Methodology sharing. For people to understand the code and methods used so they might use aspects of it in their own work, e.g., because they are doing something partially related that isn’t exactly the same job and might like to ‘dip into’ elements of this code for inspiration.

## How to run the code

This project uses the following R packages which were developed as part of the Sheffield Tobacco and Alcohol Policy Modelling https://stapm.gitlab.io/ by the School of Health and Related Research at the University of Sheffield:

- hseclean https://stapm.github.io/hseclean/  

- tobalcepi https://stapm.github.io/tobalcepi/   

The "05_install_packages.R" script installs the required packages. After installing the packages, then prepare the survey data using the code in the folder "20_prepare_survey_data". The folder "40_calculate_attributable_fractions" contains the script "05_run_pafs.R", which runs all of the other scripts within that folder to produce the population attributable fraction estimates. To generate the plots, run the scripts in "60_figures_and_tables.R".    

## How to cite the report, code and population attributable fraction estimates

Leeming, G., Angus, C. and Gillespie, D. (2023) Alcohol and Tobacco Attributable Fractions for Scotland. The University of Sheffield. Doi: 10.15131/shef.data.24543343   

## Contact

Lead analyst: Grace Leeming g.leeming@sheffield.ac.uk




