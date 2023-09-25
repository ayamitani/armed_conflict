matmor0 <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)
neomor0 <- read.csv(here("data", "original", "neonatalmortality.csv"), header = TRUE)
infmor0 <- read.csv(here("data", "original", "infantmortality.csv"), header = TRUE)
un5mor0 <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)

### write a function that does the above manipulation to each data
wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(Country.Name, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(Country.Name, year)
}

matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

#put all data frames into list
wblist <- list(matmor, infmor, neomor, un5mor)

#merge all data frames in list
wblist |> reduce(full_join, by = c('Country.Name', 'year')) -> wbdata


# add ISO-3 to data
wbdata$ISO <- countrycode(wbdata$Country.Name, 
                          origin = "country.name", 
                          destination = "iso3c")
wbdata <- wbdata |>
  dplyr::select(-Country.Name)