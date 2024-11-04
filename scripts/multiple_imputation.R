
### read in data

finaldata <- read.csv(here("data", "analytical", "finaldata.csv"))

finaldata <- finaldata |>
  mutate(loggdp = log(gdp1000),
         pctpopdens = popdens/100) |>
  select(-popdens, -gdp1000)

head(finaldata)


### Create data set used for MI

midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO, -region, -OECD2023)

### Dry run

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "pctpopdens"), "ISOnum"] <- -2


### perform MI

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)


### MI analysis

fit.mi.matmor <- with(mice.multi.out, 
                      lm(matmor ~ -1 + armconf1 + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
fit.mi.infmor <- with(mice.multi.out, 
                      lm(infmor ~ -1 + armconf1 + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
fit.mi.neomor <- with(mice.multi.out, 
                      lm(neomor ~ -1 + armconf1 + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))
fit.mi.un5mor <- with(mice.multi.out, 
                      lm(un5mor ~ -1 + armconf1 + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(year)))

out.matmor <- pool(fit.mi.matmor)
out.infmor <- pool(fit.mi.infmor)
out.neomor<- pool(fit.mi.neomor)
out.un5mor <- pool(fit.mi.un5mor)

### CC analysis

preds <- as.formula(" ~ -1 + armconf1 + loggdp + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  as.factor(ISO) + as.factor(year)")

matmormod <- lm(update.formula(preds, matmor ~ .), data = finaldata)
un5mormod <- lm(update.formula(preds, un5mor ~ .), data = finaldata)
infmormod <- lm(update.formula(preds, infmor ~ .), data = finaldata)
neomormod <- lm(update.formula(preds, neomor ~ .), data = finaldata)

tosave <- list(out.matmor, out.infmor, out.neomor, out.un5mor, 
               matmormod, un5mormod, infmormod, neomormod)

keepvars <- list("armconf1" = "Armed conflict",
                 "loggdp" = "log(GDP)",
                 "OECD" = "OECD",
                 "pctpopdens" = "Population density",
                 "urban" = "Urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall" = "Average rainfall",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")
screenreg(tosave, 
       ci.force = TRUE,
       custom.coef.map = keepvars,
       custom.model.names = c("Mat CC", "Mat MI", "Un5 CC", "Un5 MI", "Inf CC", "Inf MI", "Neo CC", "Neo MI"))

save(tosave, file = here("output", "mi_output.Rdata"))
