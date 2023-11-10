
# The aim of this code is to estimate the fractions of disease attributable to alcohol - mortality

# Load packages
library(data.table)
library(tobalcepi)
library(magrittr)
library(stapmr)
library(openxlsx)
library(dplyr)
library(tidyr)

years <- 2008:2019
substance <- "alcohol"
subtance_handel <- "alc"
date <- Sys.Date()
mort_or_morb <- "mort"
country <- "Scotland"

# template for the PAF summary sheet 
template <- "templates/PAF_summary_template_18-12-2022.xlsx"

# Load the spreadsheet (for outputs for all tables)
wb <- openxlsx::loadWorkbook(template)

# Spreadsheet cover sheet
openxlsx::writeData(wb, sheet = "Cover sheet", "Duncan Gillespie", startCol = 2, startRow = 4)
openxlsx::writeData(wb, sheet = "Cover sheet", "duncan.gillespie@sheffield.ac.uk", startCol = 2, startRow = 5)
openxlsx::writeData(wb, sheet = "Cover sheet", date, startCol = 2, startRow = 6)
openxlsx::writeData(wb, sheet = "Cover sheet", country, startCol = 2, startRow = 7)
openxlsx::writeData(wb, sheet = "Cover sheet", paste(min(years), "to", max(years)), startCol = 2, startRow = 8)
openxlsx::writeData(wb, sheet = "Cover sheet", "Alcohol", startCol = 2, startRow = 9)

openxlsx::writeData(wb, sheet = "Cover sheet", as.character(packageVersion("tobalcepi")), startCol = 2, startRow = 11)
openxlsx::writeData(wb, sheet = "Cover sheet", as.character(packageVersion("hseclean")), startCol = 2, startRow = 12)

openxlsx::writeData(wb, sheet = "Cover sheet", shell("git config --get remote.origin.url", intern = T), startCol = 2, 
                    startRow = 13)


# Load the survey data on drinking
data <- readRDS("30_intermediate_data/tobalc_consumption_scot_national_2008-2019_v1_2023-02-09_hseclean_1.10.0_imputed.rds")
data_upshift <- readRDS("30_intermediate_data/tobalc_consumption_scot_national_2008-2019_v1_2023-03-31_hseclean_1.11.1_upshifted.rds")

# Correction for partial acute alcohol aaf error - convert height from cm to m 
data[ , height := height / 100]
data_upshift[ , height := height / 100]

# Cap consumption at 300 units per week
data <- data[!(data$weekmean >= 300)]
data_upshift <- data_upshift[!(data_upshift$weekmean >= 300)]

data <- data[age >= 16 & age <= 89]
data_upshift <- data_upshift[age >= 16 & age <= 89]

# Make an ageband category
data[ , ageband := c("16-17", "18-24", "25-34", "35-49", "50+")[findInterval(age, c(-1, 18, 25, 35, 50))]]
data_upshift[ , ageband := c("16-17", "18-24", "25-34", "35-49", "50+")[findInterval(age, c(-1, 18, 25, 35, 50))]]

### merge in Oesophageal cancer splits

# ageband_scc
data[ , ageband_scc := c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                         "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
      [findInterval(age, c(-1, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85))]]


data_upshift[ , ageband_scc := c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                         "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
      [findInterval(age, c(-1, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85))]]


data_scc <- readxl::read_excel(("X:/ScHARR/PR_Disease_Risk_TA/General/CRUK work/Oesophageal AC and SCC data v2.xlsx"),
                               range = "A4:E24") %>% setDT


# change column names
# names(data_scc)
names(data_scc)[names(data_scc) == "AC as % of all oesophagus"] <- "AC_%_Male"
names(data_scc)[names(data_scc) == "SCC as % of all oesophagus"] <- "SCC_%_Male"
names(data_scc)[names(data_scc) == "...3"] <- "AC_%_Female"
names(data_scc)[names(data_scc) == "...5"] <- "SCC_%_Female"

# Remove row 1 and agebands not needed (<15-19)
data_scc <- data_scc[-c(1:4,20),]

names(data_scc)

# reshape data
# split into male and female
data_scc_male <- subset(data_scc, select = c("Age", "AC_%_Male" , "SCC_%_Male" ))
data_scc_female <-  subset(data_scc, select = c("Age", "AC_%_Female" , "SCC_%_Female"))


# create sex col

data_scc_male$sex <- "Male"
data_scc_female$sex <- "Female"

# Rename cols

names(data_scc_male)[names(data_scc_male) == "AC_%_Male"] <- "prop_ac"
names(data_scc_male)[names(data_scc_male) == "SCC_%_Male"] <- "prop_scc"

names(data_scc_female)[names(data_scc_female) == "AC_%_Female"] <- "prop_ac"
names(data_scc_female)[names(data_scc_female) == "SCC_%_Female"] <- "prop_scc"

# join male & female datasets
data_scc_2 <- rbind(data_scc_male, data_scc_female)

# rename age col to match data
names(data_scc_2)[names(data_scc_2) == "Age"] <- "ageband_scc"

data_scc_2$prop_ac <- as.numeric(data_scc_2$prop_ac)
data_scc_2$prop_scc <- as.numeric(data_scc_2$prop_scc)


# merge into data
data <- merge(data, data_scc_2, by = c("ageband_scc", "sex"),
              all = TRUE)

data_upshift <- merge(data_upshift, data_scc_2, by = c("ageband_scc", "sex"),
              all = TRUE)

# Run the below for different combinations of age, sex and IMD quintile

## 5 year condition specific estimates - no stratification

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = (max(years)-4):max(years), pool = TRUE, 
               subgroups = NULL, mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

# without protective effects
est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = (max(years)-4):max(years), pool = TRUE, 
                          subgroups = NULL, mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

# Upshifted estimates
est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = (max(years)-4):max(years), pool = TRUE, 
                       subgroups = NULL, mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

# Upshifted without protective effects
est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = (max(years)-4):max(years), pool = TRUE, 
                                  subgroups = NULL, mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("year", "condition"))   %>%
  full_join(., est_upshift, by = c("year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

est[ , year := as.character(year)]
est[ , year := paste(max(years)-4, "to", max(years), "pooled")]

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "5 year Condition", 
                     title = "Table 1. Alcohol Attributable Fractions for mortality. For a pooled sample of 2015 to 2019.", 
                     est, startCol = 1, startRow = 1)


## 5 year condition specific estimates - by ageband

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = (max(years)-4):max(years), pool = TRUE, 
               subgroups = c("ageband"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = (max(years)-4):max(years), pool = TRUE, 
                          subgroups = c("ageband"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = (max(years)-4):max(years), pool = TRUE, 
                       subgroups = c("ageband"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = (max(years)-4):max(years), pool = TRUE, 
                                  subgroups = c("ageband"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("ageband", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("ageband", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("ageband", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

est[ , year := as.character(year)]
est[ , year := paste(max(years)-4, "to", max(years), "pooled")]

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")


stapmr::WriteToExcel(wb, sheet = "5 year Age band", 
                     title = "Table 2. Alcohol Attributable Fractions for mortality by age-band. For a pooled sample of 2015 to 2019.", 
                     est, startCol = 1, startRow = 1)


## 5 year condition specific estimates - by sex

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = (max(years)-4):max(years), pool = TRUE, 
               subgroups = c("sex"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = (max(years)-4):max(years), pool = TRUE, 
                          subgroups = c("sex"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = (max(years)-4):max(years), pool = TRUE, 
                       subgroups = c("sex"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = (max(years)-4):max(years), pool = TRUE, 
                                  subgroups = c("sex"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("sex", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("sex", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("sex", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

est[ , year := as.character(year)]
est[ , year := paste(max(years)-4, "to", max(years), "pooled")]

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "5 year Sex", 
                     title = "Table 3. Alcohol Attributable Fractions for mortality by sex. For a pooled sample of 2015 to 2019.", 
                     est, startCol = 1, startRow = 1)


## 5 year condition specific estimates - by IMD quintile

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = (max(years)-4):max(years), pool = TRUE, 
               subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = (max(years)-4):max(years), pool = TRUE, 
                          subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = (max(years)-4):max(years), pool = TRUE, 
                       subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = TRUE, use_weights = TRUE, 
                                  year_range = (max(years)-4):max(years), pool = TRUE, 
                                  subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)



# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("imd_quintile", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("imd_quintile", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("imd_quintile", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

est[ , year := as.character(year)]
est[ , year := paste(max(years)-4, "to", max(years), "pooled")]

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "5 year IMD quintile", 
                     title = "Table 4. Alcohol Attributable Fractions for mortality by Index of Multiple Deprivation quintiles. For a pooled sample of 2015 to 2019.", 
                     est, startCol = 1, startRow = 1)


## 5 year condition specific estimates - by sex, ageband and IMD quintile

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = (max(years)-4):max(years), pool = TRUE, 
               subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = (max(years)-4):max(years), pool = TRUE, 
                          subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = (max(years)-4):max(years), pool = TRUE, 
                       subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = (max(years)-4):max(years), pool = TRUE, 
                                  subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("ageband", "sex", "imd_quintile", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("ageband", "sex", "imd_quintile", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("ageband", "sex", "imd_quintile", "year", "condition"))



# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

est[ , year := as.character(year)]
est[ , year := paste(max(years)-4, "to", max(years), "pooled")]

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "5 year Sex Age IMD", 
                     title = "Table 5. Alcohol Attributable Fractions for mortality by sex, age-band and Index of Multiple Deprivation quintiles. For a pooled sample of 2015 to 2019.", 
                     est, startCol = 1, startRow = 1)

###################################################################
## Time series condition specific estimates - no stratification

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = "all", pool = FALSE, 
               subgroups = NULL, mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = "all", pool = FALSE, 
                          subgroups = NULL, mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = "all", pool = FALSE, 
                       subgroups = NULL, mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = "all", pool = FALSE, 
                                  subgroups = NULL, mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("year", "condition"))   %>%
  full_join(., est_upshift, by = c("year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "Time series Condition", 
                     title = "Table 6. Alcohol Attributable Fractions for mortality.", est, startCol = 1, startRow = 1)


## Time series condition specific estimates - by ageband

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = "all", pool = FALSE, 
               subgroups = c("ageband"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = "all", pool = FALSE, 
                          subgroups = c("ageband"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = "all", pool = FALSE, 
                       subgroups = c("ageband"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = "all", pool = FALSE, 
                                  subgroups = c("ageband"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("ageband", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("ageband", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("ageband", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "Time series Age band", 
                     title = "Table 7. Alcohol Attributable Fractions for mortality by age-band.", 
                     est, startCol = 1, startRow = 1)


## Time series condition specific estimates - by sex

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = "all", pool = FALSE, 
               subgroups = c("sex"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = "all", pool = FALSE, 
                          subgroups = c("sex"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = "all", pool = FALSE, 
                       subgroups = c("sex"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = "all", pool = FALSE, 
                                  subgroups = c("sex"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("sex", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("sex", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("sex", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "Time series Sex", 
                     title = "Table 8. Alcohol Attributable Fractions for mortality by sex.", 
                     est, startCol = 1, startRow = 1)


## Time series condition specific estimates - by IMD quintile

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = "all", pool = FALSE,  
               subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = "all", pool = FALSE,  
                          subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = "all", pool = FALSE,  
                       subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = "all", pool = FALSE,  
                                  subgroups = c("imd_quintile"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("imd_quintile", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("imd_quintile", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("imd_quintile", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "Time series IMD quintile", 
                     title = "Table 9. Alcohol Attributable Fractions for mortality by Index of Multiple Deprivation quintiles.", 
                     est, startCol = 1, startRow = 1)


## Time series condition specific estimates - by sex, ageband and IMD quintile

# calculate the pafs
est <- PAFcalc(data = data, substance = subtance_handel, 
               alc_protective = TRUE, use_weights = TRUE, 
               year_range = "all", pool = FALSE,  
               subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
               oesoph_subtypes = TRUE)

est_protect_no <- PAFcalc(data = data, substance = subtance_handel, 
                          alc_protective = FALSE, use_weights = TRUE, 
                          year_range = "all", pool = FALSE,  
                          subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
                          oesoph_subtypes = TRUE)

est_upshift <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                       alc_protective = TRUE, use_weights = TRUE, 
                       year_range = "all", pool = FALSE,  
                       subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
                       oesoph_subtypes = TRUE)

est_upshift_protect_no <- PAFcalc(data = data_upshift, substance = subtance_handel, 
                                  alc_protective = FALSE, use_weights = TRUE, 
                                  year_range = "all", pool = FALSE,  
                                  subgroups = c("ageband", "sex", "imd_quintile"), mort_or_morb = mort_or_morb,
                                  oesoph_subtypes = TRUE)

# merge estimates together
colnames(est_protect_no)[colnames(est_protect_no) == "af"] ="af_protect_no"
colnames(est_upshift)[colnames(est_upshift) == "af"] ="af_upshift"
colnames(est_upshift_protect_no)[colnames(est_upshift_protect_no) == "af"] ="af_upshift_protect_no"

est <- full_join(est, est_protect_no, by = c("ageband", "sex", "imd_quintile", "year", "condition"))   %>%
  full_join(., est_upshift, by = c("ageband", "sex", "imd_quintile", "year", "condition")) %>% 
  full_join(., est_upshift_protect_no, by = c("ageband", "sex", "imd_quintile", "year", "condition"))

# merge with disease information
est <- merge(est, tobalcepi::disease_groups, by = "condition", all.x = T, all.y = F)

setorderv(est, "disease_type", 1)
setnames(est, "af", "Population Attributable Fraction")
setnames(est, "af_protect_no", "PAF: without protective effects")
setnames(est, "af_upshift", "Upshifted Population Attributable Fraction")
setnames(est, "af_upshift_protect_no", "Upshifted PAF: without protective effects")

stapmr::WriteToExcel(wb, sheet = "Time series Sex Age IMD", 
                     title = "Table 10. Alcohol Attributable Fractions for mortality by sex, age-band and Index of Multiple Deprivation quintiles.", 
                     est, startCol = 1, startRow = 1)



openxlsx::saveWorkbook(wb, paste0("50_outputs/PAF_summary_", substance, "_", mort_or_morb, "_", country, "_", date, ".xlsx"), overwrite = T)

