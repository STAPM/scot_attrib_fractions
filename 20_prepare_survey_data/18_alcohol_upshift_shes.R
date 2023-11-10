# This code reads the imputed health survey dataset
# and conducts the alcohol upshifting


library(data.table)
library(hseclean)


library(dplyr)
library(magrittr)

# Load the data

# choose the file output by 15_Imputation_shes.R
data <- readRDS("30_intermediate_data/tobalc_consumption_scot_national_2008-2019_v1_2023-02-09_hseclean_1.10.0_imputed.rds")

# Cap consumption at 300 units per week
data <- data[!(data$weekmean >= 300)]

# Set variables for upshifting
country <- "Scotland"
pcc_data <- "MESAS"



# Create empty dataframe
df <- data.frame()

# upshift the consumption data

for (Year in 2008:2019) {
  data_upshift <-  alc_upshift(data %>% filter(year == Year),
                               country = country,
                               year_select = Year,
                               pcc_data = pcc_data,
                               proportion = 0.8)
  
  df = rbind(df, data_upshift)
}


# Remove weekmean and rename weekmean_adj

df <- subset (df, select = -weekmean)
colnames(df)[colnames(df) == "weekmean_adj"] ="weekmean"


# note the package version so that the data can be tagged with it
ver <- packageVersion("hseclean")

write.table(df, paste0("30_intermediate_data/tobalc_consumption_scot_national_2008-2019_v1_", Sys.Date(), "_hseclean_", ver, "_upshifted.csv"), row.names = F, sep = ",")
saveRDS(df, paste0("30_intermediate_data/tobalc_consumption_scot_national_2008-2019_v1_", Sys.Date(), "_hseclean_", ver, "_upshifted.rds"))
