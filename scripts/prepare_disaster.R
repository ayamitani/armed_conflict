disaster <- read.csv(here("raw data", "IDD", "disaster.csv"), header = TRUE)

### select rows for earthquake and drought and columns for year, country, ISO, disaster type
disaster |>
  dplyr::filter(Year >= 2000 & Year <= 2019) |>
  dplyr::filter(Disaster.Type %in% c("Earthquake", "Drought")) |>
  dplyr::select(Year, ISO, Disaster.Type) |>
  rename(year = Year) |>
  group_by(year, ISO) |>
  mutate(drought0 = ifelse(Disaster.Type == "Drought", 1, 0),
         earthquake0 = ifelse(Disaster.Type == "Earthquake", 1, 0)) |>
  summarize(drought = max(drought0),
            earthquake = max(earthquake0)) |> 
  ungroup() -> disasters 

#save(disasters, file = here("data", "disasters.Rda"))
