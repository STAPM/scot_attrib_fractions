
# This script calls the other scripts in the same folder
# to make running all the different types of PAFs more efficient

# Scotland 


# source("40_calculate_attributable_fractions/30_SAF_calc_scotland.R")
source("40_calculate_attributable_fractions/40_ASAF_calc_mortality_scotland.R")
source("40_calculate_attributable_fractions/45_ASAF_calc_morbidity_scotland.R")
source("40_calculate_attributable_fractions/20_AAF_calc_mortality_scotland.R")
source("40_calculate_attributable_fractions/25_AAF_calc_morbidity_scotland.R")

