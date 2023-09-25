covs <- read.csv(here("data", "original", "covariates.csv"), header = TRUE)

source(here("scripts", "prepare_mortality.R"))
source(here("scripts", "prepare_disaster.R"))
source(here("scripts", "prepare_conflict.R"))


#put all data frames into list
alllist <- list(confdata, wbdata, disasters)

#merge all data frames in list
alllist |> reduce(full_join, by = c('ISO', 'year')) -> finaldata0

finaldata <- covs |>
  left_join(finaldata0, by = c('ISO', 'year'))

# need to fill in NAs with 0's for armconf1, drought, earthquake
finaldata <- finaldata |>
  mutate(armconf1 = replace_na(armconf1, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totdeath = replace_na(totdeath, 0))

write.csv(finaldata, file = here("data", "finaldata.csv"), row.names = FALSE)
