
# ...................................
#
# Matt Ayres, 26 Feb 2025
# birdNET detection thresholds
#       
#....................................

rm(list = ls())     # clear environment

## open packages
library(iNEXT)
library(ggplot2)
library(readxl)
library(writexl)
library(gridExtra)
library(grid)
library(tidyverse)
library(cowplot)
library(scales)
library(magick)
library(pdftools)
library(readr)

#----------------------

dir_working = "C:/Ayres/Ayres.Research/BBoP/2025.02"
subdir_to_evals = "evals"
# minThresh_default = 0.30  # default minimum for using birdNET confidence scores

dir_to_evals=paste0(dir_working,"/",subdir_to_evals)
setwd(dir_to_evals)
# List all directories in the subdirectory with evaluations
all_folders <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)

# Filter for folder names with exactly 4 characters
species_list <- all_folders[nchar(all_folders) == 4]

# Print the result
print(species_list)
nSpecies=length(species_list)

# Initialize an empty data frame with the same column names as df5
df5_all <- data.frame()

# Initialize an empty data frame with the correct column names and types
df6 <- data.frame(
  Species = character(nSpecies),
  minThresh = numeric(nSpecies),
  nEvals = integer(nSpecies),
  nTrue = integer(nSpecies),
  b0 = numeric(nSpecies),
  bo_SE = numeric(nSpecies),
  b1 = numeric(nSpecies),
  b1_SE = numeric(nSpecies),
  bScore90 = numeric(nSpecies),
  ChiSquare = numeric(nSpecies),
  df = integer(nSpecies),
  P = numeric(nSpecies),
  stringsAsFactors = FALSE
)

# Loop from 1 to nSpecies
for (s in 1:nSpecies) {
  species = species_list[s]
  inFolder=paste0(subdir_to_evals,"/",species)
  
  setwd(dir_working)
  subdir_path <- inFolder

# List all .txt files in the subdirectory
txt_files <- list.files(path = subdir_path, pattern = "\\.txt$", full.names = TRUE)

# Processed ROPI text files to replace "?" with NA

# Read all files into one data frame, ensuring consistent column types
df1 <- txt_files %>%
  set_names() %>%  # Use file names as list names
  map_df(~ read.table(.x, header = TRUE, sep = "\t", stringsAsFactors = FALSE, fill = TRUE) %>%
           mutate(
             Eval = as.numeric(Eval),  # Force Eval to numeric to prevent type mismatches
             fileName = basename(.x)  # Add file name column
           ), .id = "fileName")  # Keep only base file name


# Move the fileName column to the far left
df1 <- df1 %>% relocate(fileName)

# Extract year, area, and species from fileName
df1 <- df1 %>%
  mutate(
    fileName = str_remove(fileName, paste0("^", subdir_path, "/")),  # Remove subdir path
    fileName = str_remove(fileName, "\\.txt$"),  # Remove .txt extension
    year = word(fileName, 1, sep = "_"),  # Extract first part as year
    area = word(fileName, 2, sep = "_"),  # Extract second part as area
    species = word(fileName, 3, sep = "_")  # Extract third part as species
  ) %>%

  
  relocate(year, area, species, .before = fileName)  # Move new columns to far left

# Display the first few rows
head(df1)

# Create df2 from df1 with the new "birdNET" column
df2 <- df1 %>%
  mutate(
    birdNET = as.numeric(substr(Begin.File, 1, 4))  # Extract first 4 characters and convert to numeric
  ) %>%
  relocate(birdNET, .before = last_col(offset = 1))  # Move "birdNET" to second-to-last column

# Display the first few rows of df2
head(df2)

df2 <- df2 %>%
  arrange(birdNET)  # Sort by birdNET in ascending order

# Create a new data frame sum_df3 by grouping every 50 rows
sum_df3 <- df2 %>%
  mutate(group = ceiling(row_number() / 50)) %>%  # Assign group numbers for every 50 rows
  group_by(group) %>%
  summarise(
    nRows = n(),  # Count number of rows in each subset
    birdNET_avg = mean(birdNET, na.rm = TRUE),  # Average of birdNET in the subset
    nTrue = sum(Eval, na.rm = TRUE),  # Sum of Eval in the subset
    logit_p = log((nTrue / nRows) / (1 - (nTrue / nRows)))  # Logit transformation
  ) %>%
  mutate(logit_p = ifelse(nTrue == 0 | nTrue == nRows, NA, logit_p)) %>%  # Handle edge cases
  select(-group)  # Remove the grouping column

# Display the first few rows
head(sum_df3)

# Define output file name
outFile <- paste0("logit_all/logit_all_", species, ".jpg")

# Create scatter plot with 95% confidence interval
plot <- ggplot(sum_df3, aes(x = birdNET_avg, y = logit_p)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +  # Add confidence interval
  labs(
    x = "Average birdNET",
    y = "Logit of nTrue/nRows",
    title = paste0(species, " - all data")
  ) +
  theme_bw(base_size = 14)  # Clean theme for better visualization

# Save the plot as JPG
ggsave(filename = outFile, plot = plot, width = 7, height = 5, dpi = 300, device = "jpeg")

# Confirm the file is saved
print(paste("Plot saved as:", outFile))

# Filter df2 to exclude cases with low birdNET
# Set minThresh conditionally
# Find the minimum for each area
df2_min <- df2 %>%
  group_by(area) %>%
  summarise(min_birdNET = min(birdNET, na.rm = TRUE))

# View result
print(df2_min)

# Create histograms for each area
ggplot(df2, aes(x = birdNET)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ area, scales = "free") +  # Separate histograms for each area
  labs(title = "Histograms of birdNET for Each Area",
       x = "birdNET",
       y = "Frequency") +
  theme_bw(base_size = 14)

# Find the overall minimum value across all areas
minThresh <- min(df2_min$min_birdNET, na.rm = TRUE)
print(minThresh)

df4 <- df2 %>%
  filter(birdNET > minThresh)

df4 <- df4 %>%
  drop_na(Eval)

# Display the first few rows of df4
head(df4)

# Create a new data frame sum_df4 by grouping every 20 rows
sum_df4 <- df4 %>%
  mutate(group = ceiling(row_number() / 20)) %>%  # Assign group numbers for every 50 rows
  group_by(group) %>%
  summarise(
    nRows = n(),  # Count number of rows in each subset
    birdNET_avg = mean(birdNET, na.rm = TRUE),  # Average of birdNET in the subset
    nTrue = sum(Eval, na.rm = TRUE),  # Sum of Eval in the subset
    logit_p = log((nTrue / nRows) / (1 - (nTrue / nRows)))  # Logit transformation
  ) %>%
  mutate(logit_p = ifelse(nTrue == 0 | nTrue == nRows, NA, logit_p)) %>%  # Handle edge cases
  select(-group)  # Remove the grouping column

# Display the first few rows
head(sum_df4)

# Compute nTrue while handling NA values
nTrue <- sum(df2$Eval, na.rm = TRUE)

# Check if nTrue is greater than 20 before executing actions
if (!is.na(nTrue) && nTrue > 20) {  
  print("nTrue is greater than 20. Proceeding with actions...")
  

# Fit the generalized linear model
glm_model_simple <- glm(Eval ~ birdNET, data = df4, family = binomial(link = "logit"))

# Display the model summary
summary(glm_model_simple)

# Check if `area` has more than one unique value
df = n_distinct(df4$area) - 1
if (df > 0) {
  print("Multiple areas detected. Proceeding with the next set of code...")
  
  # Your code block goes here
  # Fit the expanded generalized linear model with area as a categorical predictor
  glm_model_full <- glm(Eval ~ birdNET + factor(area), data = df4, family = binomial(link = "logit"))
  
  # Display the model summary
  summary(glm_model_full)
  
  glm_model_reduced <- glm(Eval ~ birdNET, data = df4, family = binomial(link = "logit"))
  anova_results = anova(glm_model_reduced, glm_model_full, test = "Chisq")
  
  # Extract values from the second row of ANOVA table, = test for group effects
  chi_square <- anova_results$Deviance[2]  # Deviance value from second row
  Pvalue <- anova_results$`Pr(>Chi)`[2]  # P-value from second row
  
  # Print extracted values
  print(paste("Chi-square:", chi_square))
  print(paste("P-value:", Pvalue))
} else {
  print("Only one unique area found. Skipping the next set of code.")
}

# Extract the summary of the model
summary_glm <- summary(glm_model_reduced)

# Extract coefficients and their standard errors
coefficients_table <- summary_glm$coefficients

# View the coefficient table
# print(coefficients_table)

beta_0 <- coefficients_table[1,1]  # Intercept
beta_0_SE = coefficients_table[1,2]  # Intercept SE
beta_1 <- coefficients_table[2,1]    # Slope for binomial
beta_1_SE <- coefficients_table[2,2]    # Slope for binomial

p_target <- 0.90  # Desired probability
logit_p <- log(p_target / (1 - p_target))  # Compute logit(0.90)

birdNET_target <- (logit_p - beta_0) / beta_1  # Solve for birdNET
print(birdNET_target)

# Format numeric values
birdNET_text <- sprintf(" birdNET score for 90%% true = %.2f", birdNET_target)
area_test_text <- sprintf("Area effects: chi-square = %.2f, df = %d, P = %.3f", chi_square, df, Pvalue)

# Create text grob
text_grob <- textGrob(
  label = paste(birdNET_text, "\n", area_test_text),  # Combine both lines
  x = 0, y = 1,  # Upper-left corner (relative to the plot)
  hjust = 0, vjust = 1,  # Align text to top-left
  gp = gpar(fontsize = 12, col = "black", fontface = "bold")  # Set font style
)

# Count the number of cases and sum Eval for each unique "area"
area_counts <- df4 %>%
  group_by(area) %>%
  summarise(
    nCases = n(),              # Count number of rows per area
    nTrue = sum(Eval, na.rm = TRUE)  # Sum of Eval per area
  )

# Display the result
print(area_counts)

# Create table grob from area_counts
area_table <- tableGrob(area_counts, rows = NULL)  # Convert data frame to table graphic

# Define output file name
outFile <- paste0("logit/logit_", species, ".jpg")

# Create scatter plot with 95% confidence interval and a horizontal dashed red line
plot2 <- ggplot(sum_df4, aes(x = birdNET_avg, y = logit_p)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3, fullrange = TRUE) +  # Extend regression line
  geom_hline(yintercept = logit_p, linetype = "dashed", color = "red", size = 0.5) +  # Flat thin red dashed line
  scale_x_continuous(limits = c(minThresh, 1.0)) +  # Extend x-axis to 1.0
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +  # Ensure y-axis has 2 decimal places
  labs(
    x = "Average birdNET",
    y = "Logit of P(true)",
    title = paste0(species, " (birdNet confidence > ", minThresh, ")")
  ) +
  theme_bw(base_size = 14)  # Clean theme for better visualization


# Combine the plot and table using cowplot
plot3 <- ggdraw() +
  draw_plot(plot2, 0, 0, 1, 1) +  # Main plot (full space)
  draw_plot(area_table, 0.7, 0.15, 0.25, 0.25)  # Table in lower-right corner

# Overlay text grob onto plot3
plot4 <- ggdraw() +
  draw_plot(plot3, 0, 0, 1, 1) +  # Main plot
  draw_grob(text_grob, x = 0.12, y = 0.80, width = 0.5, height = 0.1)  # Text box in upper left

# Save the plot as JPG
ggsave(filename = outFile, plot = plot4, width = 7, height = 5, dpi = 300, device = "jpeg")

# Confirm the file is saved
print(paste("Plot saved as:", outFile))

df6$Species[s] = species
df6$minThresh[s] = minThresh
df6$nEvals[s] = nrow(df4)
df6$nTrue[s] = sum(df4$Eval)
df6$b0[s] = beta_0
df6$bo_SE[s] = beta_0_SE
df6$b1[s] = beta_1
df6$b1_SE[s] = beta_1_SE
df6$bScore90[s] = birdNET_target
df6$ChiSquare[s] = chi_square
df6$df[s] = df
df6$P[s] = Pvalue

# Create df5 with the "use" column, move 'species' to first column, and remove NA rows
df5 <- df2 %>%
  mutate(use = ifelse(birdNET < minThresh, "omit", "use")) %>%
  relocate(use, .after = 3) %>%  # Move "use" column to the fourth position
  relocate(species, .before = 1) %>%  # Move "species" to the first column
  drop_na(Eval)  # Remove any rows containing NA

# Append df5 to df5_all
df5_all <- bind_rows(df5_all, df5)

} else {
  print("nTrue is 20 or less, or there are missing values. Skipping actions.")
}

} # next species

# Save df5_all as an Excel file
write_xlsx(df5_all, "birdNET_evals_75spp.xlsx")
write_xlsx(df6, "birdNET_evals_75spp_summary.xlsx")

#######################

# Define the subdirectory containing JPGs
subdir <- "logit"

# List all JPG files in the subdirectory
jpg_files <- list.files(path = subdir, pattern = "\\.jpg$", full.names = TRUE)

# Define output PDF filename
output_pdf <- "logit_allSpp.pdf"

# Set up letter-size paper in landscape mode (11 x 8.5 inches)
pdf(output_pdf, width = 11, height = 8.5)  # Start PDF output

# Process images in groups of 4 per page
num_images <- length(jpg_files)
images_per_page <- 4  # 2x2 grid

for (i in seq(1, num_images, by = images_per_page)) {
  # Select up to 4 images for the current page
  img_subset <- jpg_files[i:min(i + images_per_page - 1, num_images)]
  
  # Load images and convert to grobs
  grobs <- lapply(img_subset, function(img) rasterGrob(image_read(img)))
  
  # Arrange images in a 2x2 layout
  grid.arrange(grobs = grobs, ncol = 2, nrow = 2)
}

# Close the PDF device
dev.off()

# Confirm the file is saved
print(paste("PDF saved as:", output_pdf))

#########################

ggplot(df6, aes(x = bScore90)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0, 1.2)) +  # X-axis from 0 to 1.2
  scale_y_continuous(limits = c(0, 8)) +    # Y-axis from 0 to 8
  labs(
    title = "birdNET confidence thresholds for 37 species", 
    x = "birdNET confidence score for 90% true",  # Updated x-axis label
    y = "Frequency"
  ) +
  theme_bw(base_size = 14)

