###############################################################################
# This R script creates the final_data.csv from the original data files
# Input:
#   data/original/maternal_mortality.csv
#   data/original/infant_mortality.csv
#   data/original/neonatal_mortality.csv
#   data/original/under5_mortality.csv
#   data/original/conflictdata.csv
#   data/original/covariates.csv
#   data/original/disaster.csv
# Output:
#   data/analytical/final_data.csv
# Author:
#   Aya Mitani
###############################################################################

### 1. Prepare world bank data

### read in all data sets

matmor0 <- read.csv("data//original//maternal_mortality.csv", header = TRUE)
infmor0 <- read.csv("data//original//infant_mortality.csv", header = TRUE)
neomor0 <- read.csv("data//original//neonatal_mortality.csv", header = TRUE)
un5mor0 <- read.csv("data//original//under5_mortality.csv", header = TRUE)


### write a function that does the above manipulation to each data

wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(iso, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year)
}

### apply function to each WB data

matmor <- wbfun(dataname = matmor0, varname = "maternal_mortality")
infmor <- wbfun(dataname = infmor0, varname = "infant_mortality")
neomor <- wbfun(dataname = neomor0, varname = "neonatal_mortality")
un5mor <- wbfun(dataname = un5mor0, varname = "under5_mortality")


### 2. Prepare disaster data

disaster <- read.csv("data//original//disaster.csv", header = TRUE)

### select rows for earthquake and drought and columns for year, country, iso, disaster type
disaster |>
  dplyr::filter(Year >= 2000 & Year <= 2019) |>
  dplyr::filter(Disaster.Type %in% c("Earthquake", "Drought")) |>
  dplyr::select(Year, ISO, Disaster.Type) |>
  rename(year = Year, iso = ISO) |> 
  group_by(year, iso) |>
  mutate(drought0 = ifelse(Disaster.Type == "Drought", 1, 0),
         earthquake0 = ifelse(Disaster.Type == "Earthquake", 1, 0)) |>
  summarize(drought = max(drought0),
            earthquake = max(earthquake0)) |> 
  ungroup() |>
  arrange(iso, year) -> disasters 

### subset data to include only the 186 countries
countries <- read.table("data//original//countries.txt", header = TRUE)
disasterdata <- disasters |>
  dplyr::filter(iso %in% countries$iso) |> ungroup()


### 3. Prepare Conflict data

conflict <- read.csv(here("data//original//conflictdata.csv"), header = TRUE)

conflict %>%
  # Outcome 1: Binary indicator of armed conflict (0 if <25, 1 if >= 25 battle related deaths) for each country-year
  group_by(iso, year) |>
  summarise(total_death = sum(best)) |>
  mutate(armed_conflict = ifelse(total_death < 25, 0, 1)) |>
  ungroup() |>
  # the exposure variable "armed_conflict" is lagged by a year in the model
  mutate(year = year + 1) |>
  dplyr::select(-total_death) -> confdata


### 4. Merge all data

covariates <- read.csv(here("data//original//covariates.csv"), header = TRUE)

#put all data frames into list
alllist <- list(confdata, matmor, infmor, un5mor, neomor, disasterdata, covariates)

#merge all data frames in list
alllist |> reduce(full_join, by = c('iso', 'year')) -> finaldata0

# need to fill in NAs with 0's for armconf1, drought, earthquake
finaldata <- finaldata0 |>
  mutate(armed_conflict = replace_na(armed_conflict, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0)) |>
  group_by(iso, year) |>
  mutate(armed_conflict = lag(armed_conflict)) |>
  ungroup()

write.csv(finaldata, file = here("data", "analytical", "final_data.csv"), row.names = FALSE)
