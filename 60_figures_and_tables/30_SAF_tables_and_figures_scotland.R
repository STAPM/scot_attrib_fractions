library(readxl)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(data.table)
library(tidyr)
# display.brewer.all()

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

## Tobacco

# Read in the data
tobacco <- read_excel(("50_outputs/PAF_summary_tobacco_Scotland_2023-08-04.xlsx"),
                      sheet = "5 year Condition",
                      range = "A2:H55")  %>% setDT

# remove prop_scc

tobacco <- tobacco[-1, ]

# change column names
names(tobacco)[names(tobacco) == "Population Attributable Fraction"] <- "PAF"
names(tobacco)[names(tobacco) == "PAF: no risk of former smoking"] <- "PAF_no_former_smoke"


# with former risk of smoking

p <- ggplot(data = tobacco,
            aes(x = reorder(name_formatted, PAF), y = PAF, fill = disease_type)) +
            geom_col() +
            geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
            scale_fill_brewer(palette = "Dark2", name="Disease Type") +
            scale_y_continuous(name = "Smoking Attributable Fractions") +
            scale_x_discrete(name = "Condition") +
            #labs(caption =  "Figure 1: SAFs for 2015-2019 pooled with the risk of former smoking included, by disease type") +
            theme_attrib_fractions() +
            coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions.png",
#     dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions.png",
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


# without former risk of smoking


p <- ggplot(data = tobacco,
            aes(x = reorder(name_formatted, PAF_no_former_smoke), y = PAF_no_former_smoke, fill = disease_type)) +
            geom_col() +
            geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
            scale_fill_brewer(palette = "Dark2", name="Disease Type") +
            scale_y_continuous(name = "Smoking Attributable Fractions",
                               limits = c(NA, 0.75),
                               breaks = c(0, 0.25, 0.5, 0.75)) +
            scale_x_discrete(name = "Condition") +
            #labs(caption = "Figure 2: SAFs for 2015-2019 pooled without the risk of former smoking included, by disease type") +
            theme_attrib_fractions() +
            coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions - no former smoking risk.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions - no former smoking risk.png",
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



# 5 Year pooled - By sex 

# Read in the data
tobacco_by_sex <- read_excel(("50_outputs/PAF_summary_tobacco_Scotland_2023-08-04.xlsx"),
                             sheet = "5 year Sex",
                             range = "A2:I108")  %>% setDT

# remove prop_scc

tobacco_by_sex <- tobacco_by_sex[-c(1,2),  ]

# change column name
names(tobacco_by_sex)[names(tobacco_by_sex) == "Population Attributable Fraction"] <- "PAF"
names(tobacco_by_sex)[names(tobacco_by_sex) == "PAF: no risk of former smoking"] <- "PAF_no_former_smoke"

# With risk of former smoking

p <- ggplot(data = tobacco_by_sex,
            aes(x = reorder(name_formatted, PAF), y = PAF)) +
            geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
            geom_point(aes(colour = sex), size = 4) +
            scale_colour_manual(values = sex_cols) +
            scale_y_continuous(name = "Smoking Attributable Fractions") +
            scale_x_discrete(name = "Condition") +
           # labs(caption = "Figure 2: SAFs for 2015-2019 pooled with the risk of former smoking included, by sex") +
            theme_attrib_fractions() +
            coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by sex.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by sex.png",
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

# without former risk of smoking

p <- ggplot(data = tobacco_by_sex,
            aes(x = reorder(name_formatted, PAF_no_former_smoke), y = PAF_no_former_smoke)) +
            geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
            geom_point(aes(colour = sex), size = 4) +
            scale_colour_manual(values = sex_cols) +
            scale_y_continuous(name = "Smoking Attributable Fractions",
                               limits = c(NA, 0.75),
                               breaks = c(0, 0.25, 0.5, 0.75)) +
            scale_x_discrete(name = "Condition") +
            # labs(caption = "Figure 2x: SAFs for 2015-2019 pooled without the risk of former smoking included, by sex") +
            theme_attrib_fractions() +
            coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by sex - no former smoking risk.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by sex - no former smoking risk.png",
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

# Read in the data
tobacco_by_IMD <- read_excel(("50_outputs/PAF_summary_tobacco_Scotland_2023-08-04.xlsx"),
                             sheet = "5 year IMD quintile",
                             range = "A2:I267")  %>% setDT

# remove prop_scc

tobacco_by_IMD <- tobacco_by_IMD[-c(1:5),  ]


# change column names
names(tobacco_by_IMD)[names(tobacco_by_IMD) == "Population Attributable Fraction"] <- "PAF"
names(tobacco_by_IMD)[names(tobacco_by_IMD) == "PAF: no risk of former smoking"] <- "PAF_no_former_smoke"


# with the risk of former smoking

p <- ggplot(data = tobacco_by_IMD,
            aes(x = reorder(name_formatted, PAF), y = PAF)) +
            geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
            geom_point(aes(colour = imd_quintile), size = 4) +
            scale_colour_manual(values = SIMD_cols) +
            scale_y_continuous(name = "Smoking Attributable Fractions",
                               limits = c(NA, NA),
                               breaks = c(0, 0.25, 0.5, 0.75)) +
            scale_x_discrete(name = "Condition") +
           # labs(caption = "Figure 3: SAFs for 2015-2019 pooled with the risk of former smoking included, by SIMD") +
            theme_attrib_fractions() +
            coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by IMD.png",
#     dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by IMD.png",
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

# without former risk of smoking

p <- ggplot(data = tobacco_by_IMD,
            aes(x = reorder(name_formatted, PAF_no_former_smoke), y = PAF_no_former_smoke)) +
            geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
            geom_point(aes(colour = imd_quintile), size = 4) +
            scale_colour_manual(values = SIMD_cols) +
            scale_y_continuous(name = "Smoking Attributable Fractions",
                               limits = c(NA, NA),
                               breaks = c(0, 0.25, 0.5, 0.75)) +
            scale_x_discrete(name = "Condition") +
           # labs(caption = "Figure 3X: SAFs for 2015-2019 pooled without the risk of former smoking included, by SIMD") +
            theme_attrib_fractions() +
            coord_flip()

# ggsave(file="60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by IMD - no former smoking risk.png",
#        dpi = 600, width = 10, height = 14, units = "in")

agg_png(
  filename = "60_figures_and_tables/Graphs - Scotland/Smoking/Smoking Attributable Fractions by IMD - no former smoking risk.png",
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


