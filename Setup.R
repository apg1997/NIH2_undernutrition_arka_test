getwd()
# install.packages("pacman")
library(pacman)

p_load("Matrix", "here", "assertthat", "xml2", "digest", "deSolve", "data.table", "fst", "minpack.lm", "lubridate", "log4r", "stringi", "tools","data.table","cowplot","patchwork","dplyr")

install.packages(here("tbmod-rpackage", "tbmod_3.4.8.tar.gz"), repos = NULL, type = "source")

rm(list=ls())

p_load(tbmod,ggplot2,dplyr)

packageVersion("tbmod")

paths = set.paths(countries   = "countries", 
                  countrycode = "IND", 
                  xml         = "test_undernutrition.xml")

output = run(paths, sample.parameters = F, output.flows = F, write.to.file = F)

TB_trends <- output$stocks

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming your dataframe is named TB_trends
# Filter out rows where the values in the "TB" column contain the substring "count"
TB_trends_filtered <- TB_trends %>%
  filter(!grepl("count", TB))

# Create the plot using the filtered dataframe
sum_by_TB <- TB_trends_filtered %>%
  group_by(TB,year) %>%
  summarize(total_value = sum(value))

# Create the plot using the summarized data
ggplot(sum_by_TB, aes(x = year, y = total_value, color = TB)) +
  geom_line(stat = "identity") +
  labs(x = "Time", y = "Population in Compartment", color = "tbmod compartment") +
  theme_minimal()


pops <- TB_trends %>%
  group_by(year) %>%
  summarise(population = sum(value))

sum_tb_values <- TB_trends %>%
  filter(TB %in% c("Ds", "Dc")) %>%
  group_by(year) %>%
  summarise(prevalence_value = sum(value))

sum_tb_values <- merge(pops,sum_tb_values,by.x = "year")

sum_tb_values$prev_per_100000 <- (sum_tb_values$prevalence_value / sum_tb_values$population)*100000

ggplot(sum_tb_values, aes(x = year, y = prev_per_100000)) +
  geom_line(stat = "identity") +
  labs(x = "Time", y = "Prevalence per 100,000") +
  theme_minimal()

