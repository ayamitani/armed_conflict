
#2. Write an R script that reads in the `maternalmortality.csv` data and does the following manipulations.

#a. Use the `select()` function in the `dplyr` package (which is one of the packages in the `tidyverse` bundle) 
# to subset the data to have only the variables `Country.Name`, `X2000` -- `X2019`.
#b. The data set is currently in a **wide** format. Use the `pivot_longer()` function to convert the data set 
#into a **long** format. So that the first and last 20 rows of the resulting data set look like this. 
#Hint: You need to select the columns `X2000` to `X2019`, **remove the prefix X** from them, 
#change the **name of the variable** to `Year`, change the **values** to `MatMor`. 
#Finally, make sure the `year` variable is stored as numeric. 


matmor <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)
matmor <- matmor |>
  dplyr::select(Country.Name, X2000:X2019) |>
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               names_prefix = "X",
               values_to = "MatMor") |>
  mutate(year = as.numeric(year))
head(matmor, 20)
tail(matmor, 20)