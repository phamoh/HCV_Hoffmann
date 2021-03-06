# HCV Dataset from Georg Hoffmann, Ralf Lichtinghagen, Frank Klawonn
# R version 3.6.3 (2020-02-29) "Holding the Windsock"
# Data source: https://archive.ics.uci.edu/ml/datasets/HCV+data
# Article associated: https://jlpm.amegroups.com/article/view/4401/5424

# Code Purpose: Create some plots to visualize the participant pool of hepatitis C patients


# Packages -----------------------------------------------------------------------------------------------------------------
library(tidyverse)    # tidyverse_1.2.1
library(ggplot2)      # ggplot2_3.3.3 
library(rmarkdown)  
library(markdown)  
library(kableExtra) 

# Dataframes ---------------------------------------------------------------------------------------------------------------

# Read input
HCV_fn <- "hcvdat0.csv"
HCV <- read.csv(HCV_fn, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))

# Rename first column
HCV <- HCV %>%
  dplyr::rename("ID" = X)

# For data display in the PDF
# Create a new dataframe to display in the RMD template
# Shuffle the dataframe and display a few lines
set.seed(33)
rows <- sample(nrow(HCV))
old_HCV <- HCV[rows, ]
old_HCV <- head(old_HCV, n = 15L)
rownames(old_HCV) <- 1:nrow(old_HCV)

# Convert "Sex" to uppercase
HCV <- HCV %>%
  mutate(Sex = str_to_upper(Sex))

# Split the diagnosis "Category" column into 2 separate columns
HCV <- HCV %>% 
  separate(Category, c("Category", "Category_type"), sep = "([=])") %>%
  mutate(Category_type = str_to_title(Category_type))

HCV$Category_type <- factor(HCV$Category_type, levels = c("Blood Donor", "Suspect Blood Donor", "Hepatitis", "Fibrosis", "Cirrhosis"))
levels(HCV$Category_type)

# Categorizing age into age groups/ranges
# Example of age range, under 9 years old, 10-19, 20-29, etc
HCV <- HCV %>% 
  mutate(Age_group = cut(Age, 
                         breaks = c(1, 9, 19, 29, 39, 49, 59, 69, 79, Inf),
                         labels=c("<9", "10+", "20+", "30+", "40+", "50+", "60+", "70+", ">80")))


# Plots ---------------------------------------------------------------------------------------------------------------

# Creating a bar graph of the count of the patients' age ranges
plot1 <- ggplot(data = HCV, aes(x = Age_group, fill = Age_group)) +
  geom_bar(stat = "count", width = 0.7) +
  scale_y_continuous(breaks = seq(0, 210, 20)) + 
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age Range",
       y = "Number of Patients") +
  theme_light() +
  theme(legend.position = "none")

# Creating a stacked bar graph of the count of the patients' age range and sex
plot2 <- ggplot(data = HCV, aes(x = Age_group, fill = Age_group)) +
  geom_bar(aes(fill = Sex), width = 0.7) +
  scale_y_continuous(breaks = seq(0, 210, 20)) + 
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Age Range of Patients",
       y = "Number of Patients",
       fill = "Sex") +
  theme_bw()


# Creating a stacked bar graph of the count of the patients' age range and disease type from blood sample
plot3 <- ggplot(data = subset(HCV, Category_type %in% c("Hepatitis", "Fibrosis", "Cirrhosis")), aes(x = Age_group, fill = Age_group)) +
  geom_bar(aes(fill = Category_type), width = 0.7) +
  scale_y_continuous(breaks = seq(0, 30, 2)) + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Age Range of Patients",
       y = "Number of Patients",
       fill = "Disease \nCategory") +
  theme_bw()


# Jitter plot of Albumin measurement (ALB) in patients with "Hepatitis", "Fibrosis", "Cirrhosis"
# Plot includes a shaded green area that represents "normal albumin range"
plot4 <- ggplot(data = subset(HCV, Category_type %in% c("Hepatitis", "Fibrosis", "Cirrhosis")), aes(x = Category_type, y = ALB, colour = Category_type)) +
  geom_jitter(aes(fill = Category_type), width = 0.25, show.legend =FALSE, na.rm = TRUE) + 
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  geom_hline(yintercept = 34) +
  geom_hline(yintercept = 54) +
  annotate("rect", xmin = 0, xmax = 4, ymin = 34, ymax = 54, alpha = .1, fill = "#32CD32") +
  labs(x = "Disease Category",
       y = "Albumin Measurements (g/L)") +
  theme_bw()


# Output
rmarkdown::render(
  input = paste("Lichtinghagen_graphs.Rmd", sep=''), 
  output_format = "pdf_document",
  output_file = paste("Lichtinghagen_graphs.pdf", sep='')
)




