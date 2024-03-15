# # Load necessary libraries
# library(pacman)
# p_load("Matrix", "here", "assertthat", "xml2", "digest", "deSolve", "data.table", "fst", "minpack.lm", "lubridate", "log4r", "stringi", "tools","data.table","cowplot","patchwork","dplyr")
#
# # Install and load the 'tbmod' package
# install.packages(here("tbmod-rpackage", "tbmod_3.4.8.tar.gz"), repos = NULL, type = "source")
library(pacman)
p_load(tbmod,tidyverse)
require(tbmod)
library(data.table)
library(ggplot2)
library(cowplot)
library(patchwork)
library(dplyr)
theme_set(theme_minimal_grid() + panel_border(color = "black"))

# Check the version of the 'tbmod' package
packageVersion("tbmod")

#library(tidyverse)

# Set paths for input and output files
paths <- set.paths(
  countries = "countries",
  countrycode = "IND",
  xml = "test_undernutrition_mixing_2021.xml"
)

# Run the 'tbmod' model
output <- run(paths, sample.parameters = FALSE, output.flows = FALSE, write.to.file = TRUE)

# Extract TB trends data from the output
TB_trends <- output$stocks

# Filter out rows where the values in the "TB" column contain the substring "count"
TB_trends_filtered <- TB_trends %>%
  filter(!str_detect(TB, "count"))

# Summarize the filtered data by TB compartment and year
sum_by_TB <- TB_trends_filtered %>%
  group_by(TB, year, RISK) %>%
  summarize(total_value = sum(value))

# Create a line plot showing population in each TB compartment over time
ggplot(sum_by_TB, aes(x = year, y = total_value, color = RISK )) +
  geom_line() +
  labs(x = "Time", y = "Population in Compartment", color = "tbmod compartment") +
  # theme_minimal() +
  facet_wrap(~TB, scales = "free_y")

# Summarize TB values by year
pops <- TB_trends %>%
  group_by(year, RISK) %>%
  summarise(population = sum(value))

# Summarize TB prevalence values (Ds and Dc compartments) by year
sum_tb_values <- TB_trends %>%
  filter(TB %in% c("Ds", "Dc", "Dm")) %>%
  group_by(year, RISK) %>%
  summarise(prevalence_value = sum(value))

# Merge the population and prevalence data
sum_tb_values <- left_join(sum_tb_values, pops, by = c("year", "RISK"))

# Calculate TB prevalence per 100,000 population
sum_tb_values <- sum_tb_values %>%
  mutate(prev_per_100000 = (prevalence_value/ population) * 100000)

# Create a line plot showing TB prevalence over time
ggplot(sum_tb_values, aes(x = year, y = prev_per_100000, color = RISK)) +
  geom_line() +
  labs(x = "Time", y = "Prevalence per 100,000") +
  # theme_gray() +
  scale_y_continuous(limits = c(0, max(sum_tb_values$prev_per_100000)))

# Create a line plot showing  Population over time
ggplot(sum_tb_values, aes(x = year, y = population, color = RISK)) +
  geom_line() +
  labs(x = "Time", y = "Population") +
  # theme_gray() +
  scale_y_continuous(limits = c(0, max(sum_tb_values$population)))


str(sum_tb_values)


library(dplyr)

# Group by year
grouped_df <- group_by(sum_tb_values, year)

# Calculate RR for each year
RR_by_year <- summarise(grouped_df,
                        RR_mild = mean(prev_per_100000[RISK == "mild"]) / mean(prev_per_100000[RISK == "normal"]),
                        RR_moderate = mean(prev_per_100000[RISK == "moderate"]) / mean(prev_per_100000[RISK == "normal"]),
                        RR_over = mean(prev_per_100000[RISK == "over"]) / mean(prev_per_100000[RISK == "normal"])
)

# Print the results
print(RR_by_year)

library(ggplot2)

# Assuming RR_by_year is already calculated as per the previous code

# Reshape data from wide to long format
RR_long <- tidyr::pivot_longer(RR_by_year, cols = starts_with("RR"), names_to = "Risk_Level", values_to = "Relative_Risk")

# Plot
ggplot(RR_long, aes(x = year, y = Relative_Risk, color = Risk_Level)) +
  geom_line() +
  labs(title = "Relative Risk of Tuberculosis by Year",
       x = "Year",
       y = "Relative Risk") 
  # theme_minimal()
