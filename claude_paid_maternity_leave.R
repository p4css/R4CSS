# Load required libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Read the Excel file
data <- read_excel("data/WORLD-MACHE_Gender_6.8.15.xls")

# Filter data for Czech Republic and reshape
czech_data <- data %>%
    filter(country == "Czech Republic") %>%
    select(starts_with("matleave_")) %>%
    tidyr::pivot_longer(
        cols = everything(),
        names_to = "year",
        values_to = "level"
    ) %>%
    mutate(
        year = as.numeric(sub("matleave_", "", year)) + 
            ifelse(as.numeric(sub("matleave_", "", year)) < 95, 2000, 1900)
    )

# Create the plot
ggplot(czech_data, aes(x = year, y = level)) +
    geom_bar(stat = "identity", width = 1) +  # width = 1 removes gaps between bars
    scale_y_continuous(
        limits = c(0, 5),
        breaks = 0:5
    ) +
    scale_x_continuous(
        breaks = seq(1995, 2013, 1),
        labels = seq(1995, 2013, 1)
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
    ) +
    labs(
        title = "Czech Republic Maternity Leave Levels (1995-2013)",
        x = "Year",
        y = "Maternity Leave Level"
    )

