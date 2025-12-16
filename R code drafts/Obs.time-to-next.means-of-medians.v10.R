rm(list = ls())     # clear environment

# Load necessary library
library(dplyr)
library(exifr)
library(tidyverse)
library(writexl)
library(tidyr)
library(readxl)
library(plotly)
library(patchwork)
library(openxlsx)

# ...............................................
# Obs.time-to-next.median.v10.R
# Graphics of vocalization rhythms
#
# 20 Oct 2024 (19 April 2025)
#
#................................................

workpath_root = 'C:/Ayres/Ayres.Research/BBoP/2025_04.MABI'   # to root directory

inFile1 = "rand_obs_long_mean.xlsx"
inFile2 = "rand_obs_long_SD.xlsx"
outFile1 = "obs.time-to-next.xlsx"
  outSheet1 = "median"
  outSheet2 = "SD"
  outSheet3 = "nSongs"
outFig1 = "stackedBars/means-of-medians.png"
outFig2 = "stackedBars/means-of-SDs.png"
outFig3 = "stackedBars/means-of_nSongs.png"
inFile3 = "ABA_Checklist.Ayres.2025.xlsx"

setwd(workpath_root)

# Read the long means sheet into a data frame
df1 <- read_excel(inFile1)

# Show me the number of records for each case of df1$sppName1
df1_counts <- df1 %>%
  count(sppName1, name = "n_records")

print(df1_counts)

# delete any rows of df1 where the number of cases per Species is less than 10
df2 <- df1 %>%
  group_by(sppName1) %>%
  filter(n() >= 10) %>%
  ungroup()

df2 <- df2 %>%
  mutate(
    median.sqrt    = sqrt(time_to_next_median),
    SD.sqrt      = sqrt(time_to_next_SD),
    nSongs.sqrt  = sqrt(nSongs)
  )

# Helper function for standard error
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# 1. For time_to_next_median
median <- df2 %>%
  group_by(sppName1) %>%
  summarise(
    median.sqrt = mean(sqrt(time_to_next_median), na.rm = TRUE),
    SE        = se(sqrt(time_to_next_median)),
    n         = sum(!is.na(time_to_next_median)),
    .groups = "drop"
  )

# 2. For time_to_next_SD
SD <- df2 %>%
  group_by(sppName1) %>%
  summarise(
    mean.sqrt = mean(sqrt(time_to_next_SD), na.rm = TRUE),
    SE        = se(sqrt(time_to_next_SD)),
    n         = sum(!is.na(time_to_next_SD)),
    .groups = "drop"
  )

# 3. For nSongs
nSongs <- df2 %>%
  group_by(sppName1) %>%
  summarise(
    mean.sqrt = mean(sqrt(nSongs), na.rm = TRUE),
    SE        = se(sqrt(nSongs)),
    n         = sum(!is.na(nSongs)),
    .groups = "drop"
  )

# Create a new workbook
wb <- createWorkbook()

# Add and write each data frame to its sheet
addWorksheet(wb, outSheet1)
writeData(wb, sheet = outSheet1, x = median)

addWorksheet(wb, outSheet2)
writeData(wb, sheet = outSheet2, x = SD)

addWorksheet(wb, outSheet3)
writeData(wb, sheet = outSheet3, x = nSongs)

# Save the workbook to file
saveWorkbook(wb, file = outFile1, overwrite = TRUE)

############## Make graphs yo compare species

# Read the summary sheet into a data frame
df3 <- median

# View the first few rows of the data frame
head(df3)

df_taxa <- read_excel(inFile3)  # Read in bird species information

### Add family name and taxonomic number to df3
df3 <- df3 %>%
  left_join(df_taxa %>% select(code4, family, taxonomicNumber),
            by = c("sppName1" = "code4")) %>%
  relocate(family, taxonomicNumber, .before = 1) %>%
  rename(taxaNum = taxonomicNumber)

#sort by taxa number
  df3 <- df3 %>%
  arrange(taxaNum)

# Rename
df3 <- df3 %>%
  rename(sppName = sppName1)

df4 <- df3 %>%
  mutate(
    median.back  = median.sqrt^2,
    CI.lower   = (median.sqrt - 2 * SE)^2,
    CI.higher  = (median.sqrt + 2 * SE)^2
  )

################

# Step 1: Arrange and identify where family changes
df4_ordered <- df4 %>%
  arrange(taxaNum) %>%
  mutate(
    row_num = row_number(),
    family_change = family != lag(family, default = first(family))
  )

# Step 2: Create spacer rows after family changes
spacer_rows <- df4_ordered %>%
  filter(lead(family_change, default = FALSE)) %>%
  transmute(
    sppName = paste0("spacer_", row_num),
    median.back = NA,
    CI.lower = NA,
    CI.higher = NA,
    n = NA,
    family = NA,
    row_num = row_num + 0.5  # Places spacer between species
  )

# Step 3: Bind the rows and sort by row_num
df4_with_spacers <- df4_ordered %>%
  select(sppName, median.back, CI.lower, CI.higher, n, family, row_num) %>%
  bind_rows(spacer_rows) %>%
  arrange(row_num)

# Step 4: Use sppName as a factor to preserve order
df4_with_spacers$sppName <- factor(df4_with_spacers$sppName, levels = df4_with_spacers$sppName)

# Blank out spacer labels
x_labels_named <- as.character(df4_with_spacers$sppName)
x_labels_named[grepl("spacer_", x_labels_named)] <- ""

# Identify the levels that should get tick marks (i.e., non-blank labels)
spp_levels <- levels(df4_with_spacers$sppName)
tick_breaks <- spp_levels[x_labels_named != ""]  # Only keep those with non-blank labels
tick_labels <- x_labels_named[x_labels_named != ""]  # Corresponding labels

medianPlot = ggplot(df4_with_spacers, aes(x = sppName, y = median.back)) +
  geom_point(shape = 16, size = 4, color = "blue", na.rm = TRUE) +
  geom_errorbar(aes(ymin = CI.lower, ymax = CI.higher), width = 0.2, na.rm = TRUE) +
  
  # Sample size text below the x-axis
  geom_text(aes(x = sppName, y = -4, label = n),
            size = 4, angle = 90, vjust = 0.3, hjust = 0.3, na.rm = TRUE) +
  
  # Only show tick marks and labels for species (not spacers)
  scale_x_discrete(breaks = tick_breaks, labels = tick_labels) +
  scale_y_continuous(
    breaks = seq(0, 60, by = 20),         # major gridlines every 20 (or adjust as needed)
    minor_breaks = seq(0, 60, by = 10)    # minor gridlines every 10
  ) +
  coord_cartesian(ylim = c(-3, 60)) +
 
  # Add CI label box
  annotate("label", x = Inf, y = Inf,
           label = "± 95% CI",
           hjust = 1.1, vjust = 1.3,
           size = 6, label.size = NA, fill = "white") +
  
  labs(
    x = "",
    y = "Time (s)",
    title = "Median time to next vocalization by bird species"
  ) +

  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11, angle = 90, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 20),
    
    # Gridlines and background
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)  # <-- this sets white PNG background
  )

medianPlot

######################


# Save the plot
ggsave(
  filename = outFig1,
  plot = medianPlot,
  width = 14,
  height = 6,
  dpi = 300
)
