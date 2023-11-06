##############################################################
# Multiple imputation using chained equation
##############################################################

library(naniar)
library(mice)


## read in data
finaldata <- read.csv(here("data", "finaldata.csv"))
head(finaldata)

## visualize missing data
finaldata |>
  arrange(year, ISO) |>
  vis_miss()

## remove variables not needed for linear regression
finaldata <- finaldata |>
  dplyr::select(-country_name, -region)


finaldata$ISOnum <- as.numeric(as.factor(finaldata$ISO))

## complete case analysis
matmormod <- lm(matmor ~ armconf1 + GDP + OECD + popdens + urban + 
                  agedep + male_edu + temp + earthquake + drought + 
                  ISO + as.factor(year), data = finaldata)
summary(matmormod)

## dry run
mice0  <- mice(finaldata[,-1], seed = 100, m = 5, maxit = 0, print = F)

## change how to impute variables to 2l.pan
meth <- mice0$method
meth[c("matmor", "infmor", "GDP", "neomor", "un5mor", "temp", "popdens", "urban", "male_edu")] <- "2l.pan"
meth

## assign which class variable to use for each missing variable
pred <- mice0$predictorMatrix
pred[c("matmor", "infmor", "GDP", "neomor", "un5mor", "temp", "popdens", "urban", "male_edu"), "ISOnum"] <- -2
pred

## run MI with 10 imputations
mice.multi.out  <- mice(finaldata[,-1], seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred, print = F)
mice.multi.out.lmer <- mice.multi.out

## check convergence
plot(mice.multi.out)

## fit linear reg model with multiply imputed data
fit.multi.pan <- with(mice.multi.out.pan, 
                      lm(matmor ~ armconf1 + GDP + OECD + popdens + urban + 
                           agedep + male_edu + temp + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
## summarize the results using rubin's rules
out.multi.pan <- pool(fit.multi.pan)
summary(out.multi.pan)


## compare complete case analysis with mi analysis
screenreg(list(matmormod, out.multi.pan))
