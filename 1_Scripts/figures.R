####************************* SETUP *************************####

# path to here
here::i_am("1_Scripts/figures.R")

# source global options
source("global_options.R")

####************************* ANALYSIS *************************####

# read in data 
df = read.csv(here("0_Data", "DALYs.csv")) %>%
  group_by(location_name) %>%
  mutate(perc = val/sum(val),
         tot = sum(val))

# US and global
df %>% 
  ggplot(aes(x = reorder(cause_name, val), y = val/1000000)) + 
  theme_opts + 
  labs(x = "", y = "Disability-adjusted life-years (DALYs)") + 
  # Allow extra space on the right for the labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_bar(stat = "identity", fill = g15c_colors[1]) + 
  geom_text(aes(label = ifelse(location_name == "Global",
                               paste(comma(round(val/1000000, 1)), "\n(", round(perc*100), "%)", sep = ""),
                               paste(comma(round(val/1000, 0)), "\n(", round(perc*100), "%)", sep = "")),
                y = val/1000000),
            hjust = -0.1) +
  coord_flip() + 
  facet_grid(. ~ location_name, scales = "free")

# just global
df %>% filter(location_name=="Global") %>%
  ggplot(aes(x = reorder(cause_name, val), y = val/1000000)) + 
  theme_opts + theme(axis.text = element_text(size = 15),
                     axis.title = element_text(size = 15),
                     title = element_text(size = 16.5)) + 
  labs(x = "", y = "DALYs (millions)") + 
  # Allow extra space on the right for the labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  geom_bar(stat = "identity", fill = g15c_colors[1]) + 
  geom_text(aes(label = ifelse(location_name == "Global",
                               paste(comma(round(val/1000000, 1)), "\n(", round(perc*100), "%)", sep = ""),
                               paste(comma(round(val/1000, 0)), "\n(", round(perc*100), "%)", sep = "")),
                y = val/1000000),
            hjust = -0.1, size = 5) +
  coord_flip() 

# export plot
ggsave(here("2_Figures", "Fig1_DALYs.png"), width = 8, height = 5, dpi = 600)

####************************* OTHER FIGURES USED IN TEXT *************************####

# US DALYs from PMS
PMS_US = df %>% filter(location_name=="United States of America" & cause_name!="Other gynecological diseases") %>% 
  arrange(-val) %>% dplyr::select(cause_name, val)
PMS_US

# urinary incontinence DALYs
# from Malani et al. - National Poll on Healthy Aging Team.pdf in 0_Data:
  # 46% of women 50+ experience incontinence and 59% use protective garments
# from IHME_GBD_2021_DISABILITY_WEIGHTS_Y2024M05D13.csv in 0_Data:
  # stress incontinence DALY weight is 0.02
# from ACSST1Y2024.S0101-2025-12-02T165549.xlsx (ACS 2024) in 0_Data:
  # 65,139,799 women 50+
UI_US = 0.02 * 0.46 * 0.59 * 65139799
UI_US
