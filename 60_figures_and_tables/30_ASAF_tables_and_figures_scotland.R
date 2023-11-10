library(readxl)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(data.table)
library(tidyr)

root_dir <- "X:/ScHARR/PR_Disease_Risk_TA/Attrib_fractions/epi_pop_attrib_fractions/"


### theme for plots

theme_attrib_fractions <- function(){ 
  
  theme_classic() %+replace% 
    
    theme(panel.grid.major = element_line(colour="grey90"),
          panel.grid.minor = element_line(colour="grey90"),
          legend.position = "top",
          plot.caption = element_text(face = "italic",
                                      hjust = 0),
          plot.caption.position = "plot",
          legend.title = element_blank()
    )
}

# colour palettes
SIMD_cols <- c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")
sex_cols <- c("#6600cc", "#00cc99")

###
# 5 year pooled

### Alcohol & Tobacco
# mortality data
alc_tob_mort <-read_excel(("50_outputs/PAF_summary_alcohol_and_tobacco_mort_Scotland_2023-09-01.xlsx"),
                          sheet = "5 year Condition",
                          range = "A2:H88")  %>% setDT

# remove prop_scc
alc_tob_mort <- alc_tob_mort[-1, ]

# rename PAF col
names(alc_tob_mort)[names(alc_tob_mort) == "Population Attributable Fraction"] <- "PAF"
names(alc_tob_mort)[names(alc_tob_mort) == "Population Attributable Fraction: tob-alc interaction"] <- "PAF_interaction"

# Remove conditions wholly attributable to alcohol 
alc_tob_mort <-subset(alc_tob_mort, disease_type != "Wholly attributable to alcohol")

# morbidity
alc_tob_morb <-read_excel(("50_outputs/PAF_summary_alcohol_and_tobacco_morb_Scotland_2023-09-01.xlsx"),
                          sheet = "5 year Condition",
                          range = "A2:H88")  %>% setDT

# remove prop_scc
alc_tob_morb <- alc_tob_morb[-1, ]

names(alc_tob_morb)[names(alc_tob_morb) == "Population Attributable Fraction"] <- "PAF"
names(alc_tob_morb)[names(alc_tob_morb) == "Population Attributable Fraction: tob-alc interaction"] <- "PAF_interaction"

# Remove conditions wholly attributable to alcohol 
alc_tob_morb <-subset(alc_tob_morb, disease_type != "Wholly attributable to alcohol")


#### Tob-Alc interaction

# Subset data for relevant conditions
alc_tob_mort_interact <- subset(alc_tob_mort, condition == "Larynx"| condition == "Oesophageal_SCC" |
                                  condition == "Pharynx"| condition == "Oral_cavity")

alc_tob_morb_interact <- subset(alc_tob_morb, condition == "Larynx"| condition == "Oesophageal_SCC" |
                                  condition == "Pharynx"| condition == "Oral_cavity")



# Plot conditions with and without interaction

alc_tob_morb_interact <- alc_tob_morb_interact %>% 
  pivot_longer(cols = c("PAF", "PAF_interaction"),
               names_to = "PAF_Type",
               values_to = "Value")


p <- alc_tob_mort_interact %>%
  pivot_longer(cols = c("PAF", "PAF_interaction"),
               names_to = "PAF_Type",
               values_to = "Value") %>%
  ggplot(aes(x = reorder(name_formatted, Value), y = Value)) +
  geom_col(aes(fill = PAF_Type),
           position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  scale_fill_manual(values = c("darkorange", "deepskyblue"),
                    labels = c("Without interaction", "With interaction")) +
  # geom_point(data = alc_tob_morb_interact, shape = 4, size = 2.5,  stroke = 1,
  #            position = position_dodge2(width = 0.9),
  #            aes(colour = "Morbidity")) + 
  # scale_color_manual(values = c("black")) +
  scale_y_continuous(name = "ASAFs") +
  scale_x_discrete(name = "Condition") +
  #labs(caption = "Figure 1") +
  theme_attrib_fractions() + 
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/ASAFs/Alcohol & Tobacco Attributable Fractions - interaction conditions only.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/ASAFs/Alcohol & Tobacco Attributable Fractions - interaction conditions only.png",
  width = 12,
  height = 14,
  units = "in",
  background = "white",
  res = 800,
  scaling = 1,
  bitsize = 8
)

print(p)
dev.off()



## by sex
### Alcohol & Tobacco
# mortality data
alc_tob_mort_by_sex <-read_excel(("50_outputs/PAF_summary_alcohol_and_tobacco_mort_Scotland_2023-09-01.xlsx"),
                                 sheet = "5 year Sex",
                                 range = "A2:I174")  %>% setDT


# morbidity
alc_tob_morb_by_sex <-read_excel(("50_outputs/PAF_summary_alcohol_and_tobacco_morb_Scotland_2023-09-01.xlsx"),
                                 sheet = "5 year Sex",
                                 range = "A2:I174")  %>% setDT

# remove prop_scc
alc_tob_mort_by_sex <- alc_tob_mort_by_sex[-c(1,2),  ]
alc_tob_morb_by_sex <- alc_tob_morb_by_sex[-c(1,2),  ]

alc_tob_mort_by_sex$mort_morb <- "Mortality"
alc_tob_morb_by_sex$mort_morb <- "Morbidity"


alc_tob_by_sex <- rbind(alc_tob_mort_by_sex, alc_tob_morb_by_sex)

# Change PAF col name
names(alc_tob_by_sex)[names(alc_tob_by_sex) == "Population Attributable Fraction"] <- "PAF"

# Remove conditions wholly attributable to alcohol 
alc_tob_by_sex <-subset(alc_tob_by_sex, disease_type != "Wholly attributable to alcohol")

# ASAF only

alc_tob_by_sex_ASAF <- subset(alc_tob_by_sex, disease_type2 == "alcohol and tobacco")

p <- ggplot(data = alc_tob_by_sex_ASAF,
            aes(x = reorder(name_formatted, PAF), y= PAF )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth  = 0.75) +
  geom_point(aes(colour = sex, shape = mort_morb), size = 4) +
  scale_colour_manual(breaks = c("Female", "Male"),
                      values=c(sex_cols)) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Alcohol & Smoking Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
  # labs(tag = "Figure 1: ASAFs for mortality & morbidity, 2015-2019 pooled, by sex") +
  theme_attrib_fractions() +
  coord_flip()


# ggsave(file="60_figures_and_tables/Graphs - Scotland/ASAFs/Alcohol & Tobacco Attributable Fractions by sex.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/ASAFs/Alcohol & Tobacco Attributable Fractions by sex.png",
  width = 12,
  height = 14,
  units = "in",
  background = "white",
  res = 800,
  scaling = 1,
  bitsize = 8
)

print(p)
dev.off()

### by SIMD
### Alcohol & Tobacco
# mortality data
alc_tob_mort_by_IMD <-read_excel(("50_outputs/PAF_summary_alcohol_and_tobacco_mort_Scotland_2023-09-01.xlsx"),
                                 sheet = "5 year IMD quintile",
                                 range = "A2:I432")  %>% setDT


# morbidity
alc_tob_morb_by_IMD <-read_excel(("50_outputs/PAF_summary_alcohol_and_tobacco_morb_Scotland_2023-09-01.xlsx"),
                                 sheet = "5 year IMD quintile",
                                 range = "A2:I432")  %>% setDT

# remove prop_scc
alc_tob_mort_by_IMD <- alc_tob_mort_by_IMD[-c(1:5),  ]
alc_tob_morb_by_IMD <- alc_tob_morb_by_IMD[-c(1:5),  ]


alc_tob_mort_by_IMD$mort_morb <- "Mortality"
alc_tob_morb_by_IMD$mort_morb <- "Morbidity"


alc_tob_by_IMD <- rbind(alc_tob_mort_by_IMD, alc_tob_morb_by_IMD)

# Change PAF col name
names(alc_tob_by_IMD)[names(alc_tob_by_IMD) == "Population Attributable Fraction"] <- "PAF"

# Remove conditions wholly attributable to alcohol 
alc_tob_by_IMD <-subset(alc_tob_by_IMD, disease_type != "Wholly attributable to alcohol")

# ASAFs only

alc_tob_by_IMD_ASAF <- subset(alc_tob_by_IMD, disease_type2 == "alcohol and tobacco")

p <- ggplot(data = alc_tob_by_IMD_ASAF,
            aes(x = reorder(name_formatted, PAF), y= PAF )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = imd_quintile, shape = mort_morb), size = 4) +
  scale_colour_manual(values = SIMD_cols) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Alcohol & Smoking Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
#  labs(tag = "Figure 1: ASAFs for mortality & morbidity, 2015-2019 pooled, by SIMD" ) +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/ASAFs/Alcohol & Tobacco Attributable Fractions by IMD.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/ASAFs/Alcohol & Tobacco Attributable Fractions by IMD.png",
  width = 12,
  height = 14,
  units = "in",
  background = "white",
  res = 800,
  scaling = 1,
  bitsize = 8
)

print(p)
dev.off()



# Tobacco, Alcohol and Alcohol & Tobacco on one plot

# Tobacco data
tobacco <- read_excel(("50_outputs/PAF_summary_tobacco_Scotland_2023-08-04.xlsx"),
                      sheet = "5 year Condition",
                      range = "A2:H55")  %>% setDT

# remove prop_scc
tobacco <- tobacco[-1, ]

# change column name
names(tobacco)[names(tobacco) == "Population Attributable Fraction"] <- "PAF"


# Alcohol data - mortality
alc_mort <- read_excel(("50_outputs/PAF_summary_alcohol_mort_Scotland_2023-09-01.xlsx"),
                       sheet = "5 year Condition",
                       range = "A2:J51")  %>% setDT

# remove prop_scc
alc_mort <- alc_mort[-1, ]

# remove conditions wholly attributable to alcohol & rename column
alc_mort <-subset(alc_mort, disease_type != "Wholly attributable to alcohol")
names(alc_mort)[names(alc_mort) == "Population Attributable Fraction"] <- "PAF"

# create new variable with label
tobacco$type <- "smoking"

alc_mort$type <- "alcohol"

alc_tob_mort$type <- "alcohol and smoking"


# remove Upshifted/no risk of former smoking colummns from data 
alc_mort <- alc_mort[,  -c(4,5,6)]

tobacco <- tobacco [, -4]

alc_tob_mort <- alc_tob_mort [, -4]


# join data

data <- rbind(tobacco, alc_mort, alc_tob_mort)

# alcohol & tobacco only 
data_2 <- subset(data, disease_type2 == "alcohol and tobacco")



# Alcohol and Tobacco conditions only
p <- ggplot(data = data_2,
            aes(y = PAF, x = reorder(name_formatted, PAF), fill = type)) +
  geom_bar(position="dodge", stat="identity") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  scale_fill_brewer(palette = "Dark2", name = "Disease Type") +
  scale_y_continuous(name = "Alcohol & Smoking Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(tag = "Figure 1: ASAFs for mortality, 2015-2019 pooled, by disease type") +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/ASAFs/Population Attributable Fractions.png",
#     dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/ASAFs/Population Attributable Fractions.png",
  width = 12,
  height = 14,
  units = "in",
  background = "white",
  res = 800,
  scaling = 1,
  bitsize = 8
)

print(p)
dev.off()
