matmor <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)

matmor |>
  dplyr::select(Country.Name, X2000:X2019) |>
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               names_prefix = "X",
               values_to = "matmor") |>
  mutate(year = as.numeric(year)) |>
  arrange(Country.Name, year)

### comment