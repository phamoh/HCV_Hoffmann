# Missing Documents Report: Blinded PHI - Local
# R version 3.6.3 (2020-02-29) "Holding the Windsock"
# Data source: https://archive.ics.uci.edu/ml/datasets/HCV+data
# Article associated: https://jlpm.amegroups.com/article/view/4401/5424


# Packages -----------------------------------------------------------------------------------------------------------------
library(tidyverse)    # tidyverse_1.2.1
library(ggplot2)      # ggplot2_3.3.3 
library(RColorBrewer)

# Dataframes ---------------------------------------------------------------------------------------------------------------

# Read input
HCV_fn <- "hcvdat0.csv"
HCV <- read.csv(HCV_fn, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))

# Rename first column
HCV <- HCV %>%
  dplyr::rename("ID" = X)

# Convert "Sex" to uppercase
HCV <- HCV %>%
  mutate(Sex = str_to_upper(Sex))

# Split the diagnosis "Category" column into 2 separate columns 
HCV <- HCV %>% 
  separate(Category, c("Category", "Category_type"), sep = "([=])") %>%
  mutate(Category_type = str_to_title(Category_type))

# Categorizing age into age groups/ranges
HCV <- HCV %>% 
  mutate(Age_group = cut(Age, 
                         breaks = c(1, 9, 19, 29, 39, 49, 59, 69, 79, Inf),
                         labels=c("<9", "10+", "20+", "30+", "40+", "50+", "60+", "70+", ">80")))


# Plots ---------------------------------------------------------------------------------------------------------------

#### Age range ####

# Creating a bar graph of the count of the patients' age ranges
ggplot(data = HCV, aes(x = Age_group, fill=Age_group)) +
  geom_bar(stat = "count") +
  scale_y_continuous(breaks = seq(0, 210, 20)) + 
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age Range",
       y = "Number of Participants") +
  theme_light() +
  theme(legend.position = "none")

# Creating a stacked bar graph of the count of the patients' age range and sex
ggplot(data = HCV, aes(x = Age_group, fill=Age_group)) +
  geom_bar(aes(fill = Sex), width = 0.7) +
  scale_y_continuous(breaks = seq(0, 210, 20)) + 
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Age Range of Participants",
       y = "Number of Participants",
       fill = "Sex") +
  theme_bw()


