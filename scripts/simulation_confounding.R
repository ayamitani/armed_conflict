library(SimDesign)
?SimFunctions

# logit(Pr(X=1)) = alpha0 + alpha1 * Z
# logit(Pr(Y=1)) = beta0 + beta1 * X + beta2 * Z
# X - coffee drinker
# Z - smoker
# Y - lung cancer

###################################################
## Traditional way of running simulations
###################################################

## set simulation parameters
n <- 1000        # sample size
pz <- 0.2        # probability of Z = 1
alpha0 <- -1    
alpha1 <- 2
beta0 <- -3
beta1 <- 0
beta2 <- 2
simnum <- 1000   # number of iterations to run in the simulation
unadj.est <- adj.est <- unadj.se <- adj.se <- unadj.p <- adj.p <- rep(NA, simnum) # empty vectors to store the values
for(s in 1:simnum){  
  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  ## repeat above to randomly generate binary variable Y
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  ## combine three random variables into a data frame 
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  ## save coefficient ests, SEs, and p-values for coffee from both models in a vector
  unadj.p[s] <- unadj.coef[2,4]
  adj.p[s] <- adj.coef[2,4] 
  print(s)
}

mean(ifelse(unadj.p < 0.05, 1, 0))
mean(ifelse(adj.p < 0.05, 1, 0))





######################################################################
## Better way of running simulations using SimDesign package
######################################################################


Design <- createDesign(sample_size = c(50, 100, 500),
                       pz = c(0.2, 0.8),
                       alpha0 = c(-1, 0, 1),
                       alpha1 = c(0, 0.5, 1, 2))


Generate <- function(condition, fixed_objects = NULL){
  alpha0 <- condition$alpha0
  alpha1 <- condition$alpha1
  pz <- condition$pz
  n <- condition$sample_size
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  z <- rbinom(n, size = 1, prob = pz)
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  x <- rbinom(n, size = 1, prob = px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  dat
}

Analyse.unadj <- function(condition, dat, fixed_objects = NULL){
  glmout <- glm(lung ~ coffee, data = dat, family = "binomial")
  beta <- coef(glmout)[2]
  pval <- summary(glmout)$coef[2,4]
  ret <- c(unadj = unname(pval))
  ret
}

Analyse.adj <- function(condition, dat, fixed_objects = NULL){
  glmout <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  beta <- coef(glmout)[2]
  pval <- summary(glmout)$coef[2,4]
  ret <- c(adj = unname(pval))
  ret
}

Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- EDR(results, alpha = 0.05)
  ret
}

res <- runSimulation(design = Design, replications = 1000,
                     generate = Generate, analyse = list(Analyse.unadj, Analyse.adj), 
                     summarise=Summarise, save_results = TRUE)
res


## Create a figure that summarizes the results

reslong <- res %>%
  pivot_longer(cols = c("unadj", "adj"),
               names_to = "model",
               values_to = "edr")


reslong$sample_size <- factor(reslong$sample_size, levels = c(50,100,500),
                              ordered = TRUE, labels=c("n == 50", "n == 100", "n == 500"))
reslong$alpha0 <- factor(reslong$alpha0, levels = c(-1,0,1),
                         ordered = TRUE, labels=c("alpha[0] == -1", "alpha[0] == 0", "alpha[0] == 1"))


ggplot(reslong, aes(x = alpha1, y = edr, color = model, 
                    linetype = as.factor(pz), shape = as.factor(pz), 
                    group = interaction(model, pz))) +
  geom_point(size = 2) + 
  geom_line(linewidth = 1) + 
  scale_color_discrete(labels=c('Adjusted', 'Unadjusted')) + 
  facet_grid(sample_size ~ alpha0, labeller = label_parsed) + 
  theme_bw(base_size = 12) + 
  geom_hline(yintercept = 0.05, color = "darkgray", linetype = "longdash") + 
  labs(y = "Type I error", x = bquote(alpha[1]~values), color = "Model", shape = "Pr(Z=1)", linetype = "Pr(Z=1)", 
       title = "Empirical type I error rates for adjusted and unadjusted models \nfrom each simulation scenario (gray dashed horizontal line at 0.05) ")
