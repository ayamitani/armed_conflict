##############################################################
# Multiple imputation using chained equation
##############################################################


library(naniar)
library(mice)

finaldata <- read.csv(here("data", "analytical", "final_data.csv"))

## log-transform GDP, save iso as numeric

midata <- finaldata |>
  mutate(loggdp = log(gdp_1000),
         iso_num = as.numeric(as.factor(finaldata$iso))) |>
  select(-iso)

## Dry run to get `meth` and `pred`

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

##  Edit `meth` and `pred`

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality"), "iso_num"] <- -2



## MI with mice package

## Perform MI with `m=2` imputations

mice.multi.out  <- mice(midata, seed = 100, m = 2, maxit = 3,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)


fit.mi.matmor <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
out.mi.matmor <- pool(fit.mi.matmor)
summaey(our.mi.matmor)


























head(finaldata)

finaldata |>
  arrange(year, ISO) |>
  vis_miss()

finaldata <- finaldata |>
  dplyr::select(-country_name, -region, -v2x_polyarchy, -EFindex, -rainfall)

finaldata |>
  arrange(year, ISO) |>
  vis_miss()

finaldata$ISOnum <- as.numeric(as.factor(finaldata$ISO))

matmormod <- lm(MatMor ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                  AgeDep + male_edu + temp + earthquake + drought + 
                  ISO + as.factor(year), data = finaldata)
summary(matmormod)


mice0  <- mice(finaldata[,-c(1:2)], seed = 100, m = 5, maxit = 0, print = F)
meth <- mice0$method
meth[c("MatMor", "InfMor", "gdp1000", "NeoMor", "Un5Mor", "temp", "rainfall1000", "popdens", "urban", "male_edu")] <- "2l.lmer"
meth

pred <- mice0$predictorMatrix
pred[c("MatMor", "InfMor", "gdp1000", "NeoMor", "Un5Mor", "temp", "rainfall1000", "popdens", "urban", "male_edu"), "ISOnum"] <- -2
pred

mice.multi.out  <- mice(finaldata[,-c(1:2)], seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
mice.multi.out.lmer <- mice.multi.out
plot(mice.multi.out)


fit.mi.matmor <- with(mice.multi.out, 
                   lm(MatMor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban + 
                        AgeDep + male_edu + temp + rainfall1000 + earthquake + drought + 
                        as.factor(ISOnum) + as.factor(year)))
out.mi.matmor <- pool(fit.mi.matmor)
summary(out.mi.matmor)

fit.mi.un5mor <- with(mice.multi.out, 
                      lm(Un5Mor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban + 
                           AgeDep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
out.mi.un5mor <- pool(fit.mi.un5mor)
summary(out.mi.un5mor)

fit.mi.neomor <- with(mice.multi.out, 
                      lm(NeoMor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban + 
                           AgeDep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
out.mi.neomor <- pool(fit.mi.neomor)
summary(out.mi.neomor)

fit.mi.infmor <- with(mice.multi.out, 
                      lm(InfMor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban + 
                           AgeDep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
out.mi.infmor <- pool(fit.mi.infmor)
summary(out.mi.infmor)




screenreg(list(out.mi.matmor, out.mi.infmor, out.mi.neomor, out.mi.un5mor))


fit.multi.pan <- with(mice.multi.out, 
                      plm(MatMor ~ + armconf1 + gdp1000 + OECD + popdens + urban + 
                           AgeDep + male_edu + temp + rainfall1000 + earthquake + drought, 
                          index = c("ISOnum", "year"), effect = "twoways",
                          model = "within", data = finaldata))
out.multi.pan <- pool(fit.multi.pan)
summary(out.multi.pan)