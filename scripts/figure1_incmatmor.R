
# Create a descriptive figure of maternal mortality trends for countries that had an increase from 2000 to 2017

finaldata <- read.csv(here("data", "analytical", "finaldata.csv"), header = TRUE)

forfigure <- finaldata |>
  # only select variables needed to create the figure
  dplyr::select(country_name, ISO, year, matmor) |>
  # select years 2000 to 2017
  dplyr::filter(year < 2018) |>
  # gropu by country
  group_by(ISO) |>
  # create a new variable diffmatmor that takes the difference between each year and baseline year 
  mutate(diffmatmor = matmor - matmor[1L]) |>
  # sort the data by country and decreasing year
  arrange(ISO, desc(year)) |>
  # if the first difference is greater than 0 (if year 2017 is greater than baseline, then incmatmor = 1)
  mutate(incmatmor = ifelse(diffmatmor[1L] > 0 , 1, 0)) |>
  # re-sort data by country and increasing year
  arrange(ISO, year) |>
  ungroup() |>
  # only include countries that had an increase in maternal mortality from 2000 to 2017
  dplyr::filter(incmatmor == 1)
length(unique(forfigure$ISO))

fig1 <- forfigure |>
  ggplot(aes(x = year, y = matmor, group = ISO)) +
  geom_line(aes(color = country_name), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  # use log 10 sclae for y axis
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality (log 10 scale)", x = "Year", color = "Country", title = "Trend in maternal mortality for countries that had an increase from 2000 to 2017") + 
  # use black and white theme and increase the size of labels
  theme_bw(base_size = 12)

# save the gplot as a png file
ggsave(fig1, file = here("figures", "fig1_incmatmor.png"), width = 8, height = 5)
