# # Load necessary libraries
# library(pacman)
# p_load("Matrix", "here", "assertthat", "xml2", "digest", "deSolve", "data.table", "fst", "minpack.lm", "lubridate", "log4r", "stringi", "tools","data.table","cowplot","patchwork","dplyr")
#
# # Install and load the 'tbmod' package
# install.packages(here("tbmod-rpackage", "tbmod_3.4.8.tar.gz"), repos = NULL, type = "source")
library(pacman)
p_load(tbmod)
require(tbmod)
library(data.table)
library(ggplot2)
library(cowplot)
library(patchwork)
library(dplyr)
theme_set(theme_minimal_grid() + panel_border(color = "black"))

# Check the version of the 'tbmod' package
packageVersion("tbmod")

# Set paths for input and output files
paths <- set.paths(
  countries = "countries",
  countrycode = "IND",
  xml = "countries/IND/parameters/test_undernutrition_mixing_2021.xml"
)

# Run the 'tbmod' model
output <- run(paths, sample.parameters = FALSE, output.flows = FALSE, write.to.file = TRUE)

# Extract TB trends data from the output
TB_trends <- output$stocks

# Load necessary libraries for plotting
library(ggplot2)
library(dplyr)

# Filter out rows where the values in the "TB" column contain the substring "count"
TB_trends_filtered <- TB_trends %>%
  filter(!grepl("count", TB))

# Summarize the filtered data by TB compartment and year
sum_by_TB <- TB_trends_filtered %>%
  group_by(TB, year) %>%
  summarize(total_value = sum(value))

# Create a line plot showing population in each TB compartment over time
library(ggplot2)

ggplot(sum_by_TB, aes(x = year, y = total_value, color = TB)) +
  geom_line(stat = "identity") +
  labs(x = "Time", y = "Population in Compartment", color = "tbmod compartment") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,max(sum_by_TB$total_value)))

# Summarize TB values by year
pops <- TB_trends %>%
  group_by(year) %>%
  summarise(population = sum(value))

# Summarize TB prevalence values (Ds and Dc compartments) by year
sum_tb_values <- TB_trends %>%
  filter(TB %in% c("Ds", "Dc", "Dm")) %>%
  group_by(year) %>%
  summarise(prevalence_value = sum(value))

# Merge the population and prevalence data
sum_tb_values <- merge(pops, sum_tb_values, by.x = "year")

# Calculate TB prevalence per 100,000 population
sum_tb_values$prev_per_100000 <- (sum_tb_values$prevalence_value / sum_tb_values$population) # * 100000

sum_tb_values$year <- as.integer(sum_tb_values$year)
# Create a line plot showing TB prevalence over time
ggplot(sum_tb_values, aes(x = year, y = prev_per_100000)) + geom_line() +
  labs(x = "Time", y = "Prevalence per 100,000") +
  scale_y_continuous(limits = c(0,max(sum_tb_values$prev_per_100000)))+
  theme_minimal() +
  scale_color_viridis_c()

