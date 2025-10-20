
# Create a descriptive figure of maternal mortality trends for countries that had an increase from 2000 to 2017

finaldata <- read.csv(here("data", "analytical", "final_data.csv"), header = TRUE)

forfigure <- finaldata |>
  # only select variables needed to create the figure
  dplyr::select(iso, year, maternal_mortality) |>
  # select years 2000 to 2017
  dplyr::filter(year < 2018) |>
  # gropu by country
  group_by(iso) |>
  # create a new variable diffmatmor that takes the difference between each year and baseline year 
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L]) |>
  # sort the data by country and decreasing year
  arrange(iso, desc(year)) |>
  # if the first difference is greater than 0 (if year 2017 is greater than baseline, then incmatmor = 1)
  mutate(inc_matmor = ifelse(diff_matmor[1L] > 0 , 1, 0)) |>
  # re-sort data by country and increasing year
  arrange(iso, year) |>
  ungroup() |>
  # only include countries that had an increase in maternal mortality from 2000 to 2017
  dplyr::filter(inc_matmor == 1)
length(unique(forfigure$iso))

fig01 <- forfigure |>
  ggplot(aes(x = year, y = maternal_mortality, group = iso)) +
  geom_line(aes(color = iso), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  # use log 10 sclae for y axis
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality (log 10 scale)", x = "Year", color = "Country", title = "Trend in maternal mortality for countries that had an increase from 2000 to 2017") + 
  # use black and white theme and increase the size of labels
  theme_bw(base_size = 12)

# save the gplot as a png file
ggsave(fig01, file = here("output", "fig01_increase_maternal_mortality.png"), width = 8, height = 5)
