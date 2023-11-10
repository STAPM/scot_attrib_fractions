library(readxl)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(data.table)
library(tidyr)
library(ragg)


# root_dir <- "X:/ScHARR/PR_Disease_Risk_TA/Attrib_fractions/epi_pop_attrib_fractions/"
root_dir <- "X:/ScHARR/PR_Disease_Risk_TA/Attrib_fractions/Scotland AFs/scotland_attrib_fractions"


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


# 5 year pooled


# Read in data

alc_mort <- read_excel(("50_outputs/PAF_summary_alcohol_mort_Scotland_2023-09-01.xlsx"),
                       sheet = "5 year Condition",
                       range = "A2:J50")  %>% setDT

alc_morb <- read_excel(("50_outputs/PAF_summary_alcohol_morb_Scotland_2023-09-01.xlsx"),
                       sheet = "5 year Condition",
                       range = "A2:J50")  %>% setDT


# Remove conditions wholly attributable to alcohol & rename PAF col
# Mortality
alc_mort <-subset(alc_mort, disease_type != "Wholly attributable to alcohol")
names(alc_mort)[names(alc_mort) == "Population Attributable Fraction"] <- "PAF"
names(alc_mort)[names(alc_mort) == "PAF: without protective effects"] <- "PAF_no_protect"

# Morbidity
alc_morb <-subset(alc_morb, disease_type != "Wholly attributable to alcohol")
names(alc_morb)[names(alc_morb) == "Population Attributable Fraction"] <- "PAF"
names(alc_morb)[names(alc_morb) == "PAF: without protective effects"] <- "PAF_no_protect"


# With protective effects

p <- ggplot(data = alc_mort,
            aes(x = reorder(name_formatted, PAF), y= PAF )) +
  geom_col(aes(fill = disease_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  scale_fill_brewer(palette = "Dark2", name="Disease Type") +
  geom_point(data = alc_morb, shape = 4, size = 2.5, stroke = 1,
             aes(colour = "Morbidity")) +
  scale_color_manual(values = c("black")) +
  scale_y_continuous(name = "Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
  # labs(caption = "Figure 4: AAFs for mortality & morbidity, 2015-2019 pooled with protective effects of alcohol included, by disease type") +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions.png",
#     dpi = 800, width = 12, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions.png",
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


# without protective effects

p <- ggplot(data = alc_mort,
            aes(x = reorder(name_formatted, PAF_no_protect), y= PAF_no_protect )) +
  geom_col(aes(fill = disease_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  scale_fill_brewer(palette = "Dark2", name="Disease Type") +
  geom_point(data = alc_morb, shape = 4, size = 2.5,  stroke = 1,
             aes(colour = "Morbidity")) +
  scale_color_manual(values = c("black")) + 
  scale_y_continuous(name = "Alcohol Attributable Fractions") +
                     # limits = c(-0.2, NA),
                     # breaks = c(-0.2, 0, 0.2, 0.6)) +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure X: AAFs for mortality & morbidity, 2015-2019 pooled without protective effects of alcohol, by disease type") +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions - no protective effects.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions - no protective effects.png",
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


# 5 year pooled - by sex

# Read in data

alc_mort_by_sex <- read_excel(("50_outputs/PAF_summary_alcohol_mort_Scotland_2023-09-01.xlsx"),
                              sheet = "5 year Sex",
                              range = "A2:K98")  %>% setDT

alc_morb_by_sex <- read_excel(("50_outputs/PAF_summary_alcohol_morb_Scotland_2023-09-01.xlsx"),
                              sheet = "5 year Sex",
                              range = "A2:K98")  %>% setDT

# Remove conditions wholly attributable to alcohol & rename PAF col
alc_mort_by_sex <-subset(alc_mort_by_sex, disease_type != "Wholly attributable to alcohol")
names(alc_mort_by_sex)[names(alc_mort_by_sex) == "Population Attributable Fraction"] <- "PAF"
names(alc_mort_by_sex)[names(alc_mort_by_sex) == "PAF: without protective effects"] <- "PAF_no_protect"

alc_morb_by_sex  <-subset(alc_morb_by_sex , disease_type != "Wholly attributable to alcohol")
names(alc_morb_by_sex )[names(alc_morb_by_sex ) == "Population Attributable Fraction"] <- "PAF"
names(alc_morb_by_sex)[names(alc_morb_by_sex) == "PAF: without protective effects"] <- "PAF_no_protect"

alc_mort_by_sex$mort_morb <- "Mortality"
alc_morb_by_sex$mort_morb <- "Morbidity"


alc_by_sex <- rbind(alc_mort_by_sex, alc_morb_by_sex)

# with protective effects of alcohol included

p <- ggplot(data = alc_by_sex,
            aes(x = reorder(name_formatted, PAF), y= PAF )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = sex, shape = mort_morb), size = 4) +
  scale_colour_manual(breaks = c("Female", "Male"),
                      values=c(sex_cols)) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure 5: AAFs for mortality & morbidity, 2015-2019 pooled with protective effects of alcohol included, by sex") +
  theme_attrib_fractions() +
  theme(legend.title = element_blank()) +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by sex.png",
#     dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by sex.png",
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



# without protective effects

p <- ggplot(data = alc_by_sex,
            aes(x = reorder(name_formatted, PAF_no_protect), y= PAF_no_protect )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = sex, shape = mort_morb), size = 4) +
  scale_colour_manual(breaks = c("Female", "Male"),
                      values=c(sex_cols)) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure X: AAFs for mortality & morbidity, 2015-2019 pooled without protective effects of alcohol, by sex") +
  theme_attrib_fractions() +
  theme(legend.title = element_blank()) +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by sex - no protective effects.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by sex - no protective effects.png",
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


# 5 year pooled - by SIMD

# Data
alc_mort_by_IMD <- read_excel(("50_outputs/PAF_summary_alcohol_mort_Scotland_2023-09-01.xlsx"),
                              sheet = "5 year IMD quintile",
                              range = "A2:K242")  %>% setDT

alc_morb_by_IMD <- read_excel(("50_outputs/PAF_summary_alcohol_morb_Scotland_2023-09-01.xlsx"),
                              sheet = "5 year IMD quintile",
                              range = "A2:K242")  %>% setDT

alc_mort_by_IMD$mort_morb <- "Mortality"
alc_morb_by_IMD$mort_morb <- "Morbidity"


alc_by_IMD <- rbind(alc_mort_by_IMD, alc_morb_by_IMD)

# Remove conditions wholly attributable to alcohol & rename PAF col
alc_by_IMD <-subset(alc_by_IMD, disease_type != "Wholly attributable to alcohol")
names(alc_by_IMD)[names(alc_by_IMD) == "Population Attributable Fraction"] <- "PAF"
names(alc_by_IMD)[names(alc_by_IMD) == "PAF: without protective effects"] <- "PAF_no_protect"


# with protective effects of alcohol

p <- ggplot(data = alc_by_IMD,
            aes(x = reorder(name_formatted, PAF), y= PAF )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = imd_quintile, shape = mort_morb), size = 4) +
  scale_colour_manual(values = SIMD_cols,
                      labels = c("1 - least deprived", "2", "3", "4", "5 - most deprived")) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Alcohol Attributable Fractions",
                     limits = c(NA, 0.7),
                     breaks = c(-0.2, 0, 0.2, 0.4, 0.6)) +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure 6: AAFs for mortality & morbidity, 2015-2019 pooled with protective effects of alcohol included, by SIMD") +
  theme_attrib_fractions() +
  theme(legend.title = element_blank()) +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by IMD.png",
#     dpi = 600, width = 10.5, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by IMD.png",
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



# without protective effects

p <- ggplot(data = alc_by_IMD,
            aes(x = reorder(name_formatted, PAF_no_protect), y= PAF_no_protect )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = imd_quintile, shape = mort_morb), size = 4) +
  scale_colour_manual(values = SIMD_cols,
                      labels = c("1 - least deprived", "2", "3", "4", "5 - most deprived")) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure X: AAFs for mortality & morbidity, 2015-2019 pooled without protective effects of alcohol, by SIMD") +
  theme_attrib_fractions() +
  theme(legend.title = element_blank()) +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by IMD - no protective effects.png",
#        dpi = 600, width = 10.5, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Alcohol Attributable Fractions by IMD - no protective effects.png",
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



### Upshifted ###
# with protective effects

# 5 year pooled

# rename columns
names(alc_mort)[names(alc_mort) == "Upshifted Population Attributable Fraction"] <- "PAF_upshift"
names(alc_morb)[names(alc_morb) == "Upshifted Population Attributable Fraction"] <- "PAF_upshift"

p <- ggplot(data = alc_mort,
            aes(x = reorder(name_formatted, PAF_upshift), y= PAF_upshift )) +
  geom_col(aes(fill = disease_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  scale_fill_brewer(palette = "Dark2", name="Disease Type") +
  geom_point(data = alc_morb, shape = 4, size = 2.5, stroke = 1,
             aes(colour = "Morbidity")) +
  scale_color_manual(values = c("black")) + 
  scale_y_continuous(name = "Upshifted Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure 1") +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted Alcohol Attributable Fractions.png",
#     dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted Alcohol Attributable Fractions.png",
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


# 5 year pooled - by sex

# Rename column

names(alc_by_sex)[names(alc_by_sex) == "Upshifted Population Attributable Fraction"] <- "PAF_upshift"


p <- ggplot(data = alc_by_sex,
            aes(x = reorder(name_formatted, PAF_upshift), y= PAF_upshift)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = sex, shape = mort_morb), size = 4) +
  scale_colour_manual(breaks = c("Female", "Male"),
                      values= sex_cols) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Upshifted Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
#  labs(caption = "Figure 1") +
  theme_attrib_fractions() +
  coord_flip() 

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted Alcohol Attributable Fractions by sex.png",
#     dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted Alcohol Attributable Fractions by sex.png",
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


# 5 year pooled - by SIMD

names(alc_by_IMD)[names(alc_by_IMD) == "Upshifted Population Attributable Fraction"] <- "PAF_upshift" 


p <- ggplot(data = alc_by_IMD,
            aes(x = reorder(name_formatted, PAF_upshift), y= PAF_upshift )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  geom_point(aes(colour = imd_quintile, shape = mort_morb), size = 4) +
  scale_colour_manual(values = SIMD_cols,
                      labels = c("1 - least deprived", "2", "3", "4", "5 - most deprived")) +
  scale_shape_manual(values = c(4, 16)) +
  scale_y_continuous(name = "Upshifted Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure 1") +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted Alcohol Attributable Fractions by IMD.png",
#     dpi = 600, width = 10.5, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted Alcohol Attributable Fractions by IMD.png",
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



# Non-upshifted and upshifted mortality together
# 5 yr pooled

p <- alc_mort %>% 
  pivot_longer(cols = c("PAF", "PAF_upshift"),
               names_to = "PAF_Type",
               values_to = "Value") %>%
  ggplot(aes(x = reorder(name_formatted, Value), y = Value, fill = PAF_Type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
  scale_fill_viridis_d(option = "A",
                       begin = 0,
                       end = 0.5,
                       alpha = 0.85,
                       labels = c("PAF", "PAF - upshifted")) +
  scale_y_continuous(name = "Alcohol Attributable Fractions") +
  scale_x_discrete(name = "Condition") +
 # labs(caption = "Figure 1") +
  theme_attrib_fractions() +
  coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted & non-upshifted Alcohol Attributable Fractions - Mort.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Alcohol/Upshifted & non-upshifted Alcohol Attributable Fractions - Mort.png",
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



