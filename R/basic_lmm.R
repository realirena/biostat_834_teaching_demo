Sys.setenv(
  LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"),
                         "/usr/local/lib/R/site-library/igraph/libs"),
  PKG_CFLAGS="-I/usr/local/lib/R/site-library/igraph/include",
  PKG_LIB="-L/usr/local/lib/R/site-library/igraph/libs"
)
library(lme4)
library(brms)
library(rstan)
library(afex)
library(nlme)
library(tidyverse)
library(merTools)
aid_data <- read.csv("/Users/irena/repos/biostat_834_teaching_demo/data/biostat_834_dataset.csv")
#aid_data <- read.csv("~/irena/biostat_834_demo/data/biostat_834_dataset.csv")

### turn estimated incidence into numeric: 
aid_data[aid_data$estimated_incidence=="<0.01 ",]$estimated_incidence <- 0 
aid_data[aid_data$estimated_incidence%in%c("... "),]$estimated_incidence <- NA
## try a simple LMM 

aid_data$estimated_incidence_num <- as.numeric(aid_data$estimated_incidence)
#sum(is.na(aid_data$estimated_incidence))
aid_data$dah_19_in_mill <- aid_data$sum_hiv_dah_19/10000

aid_data$dah_19_centered <- aid_data$dah_19_in_mill - mean(aid_data$dah_19_in_mill)


aid_lme <- lmer(
  estimated_incidence_num ~dah_19_centered + year + (1 + year|iso3c), 
  data=aid_data,
  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e4)))

summary(aid_lme) 

country_subset <- c("SVN", "EST","TTO","HRV", "AGO")

randomSims <- REsim(aid_lme, n.sims = 5000)

randomSims <- randomSims[randomSims$groupID%in%country_subset&randomSims$term=="year",]
# and to plot it


ggplot(randomSims, aes(x=groupID, y=mean, group=groupID, color=groupID)) + 
  geom_point(size=3) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x="Country iso3c", y="Mean Estimate")
### drop the random slope and only do the random intercept 


## try a different correlation structure? 

aid_nlme <- lme(
  estimated_incidence_num ~ dah_19_in_mill + year,
  random = ~ year|iso3c, 
  correlation = corAR1(),
  na.action = na.omit,
  data=aid_data)


summary(aid_nlme)

## try a Bayesian model: 

aid_data_noNA <- na.omit(aid_data[,c("dah_19_in_mill" ,"estimated_incidence_num", "year", "iso3c")])

## count number of obs by group 

aid_num_obs_by_country <- aid_data_noNA %>%
  group_by(iso3c) %>%
  tally()



bayes_lmm <- brm(estimated_incidence_num~ 1 + dah_19_in_mill + year + (1 + year|iso3c), 
                 data=aid_data_noNA,
                 family=gaussian(),
                 prior= c(
                   prior(normal(0, 10), class = Intercept),
                   prior(normal(0, 10), class = b),
                   prior(cauchy(0, 1), class = sigma),
                   prior(cauchy(0, 1), class = sd)),
                 warmup = 1000, 
                 iter = 2000, 
                 chains = 4, 
                 control = list(
                   adapt_delta = 0.98, 
                   max_treedepth=40)
                 )


coef(bayes_lmm)   
#saveRDS(bayes_lmm, "/Users/irena/repos/biostat_834_teaching_demo/data/bayes_lmm_example.RDS")
#bayes_lmm <- readRDS("/Users/irena/repos/biostat_834_teaching_demo/data/bayes_lmm_example.RDS")

plot(bayes_lmm, variable=c("^b", "sigma", "sd", "L"), regex = TRUE) 

summary(bayes_lmm,pars=c("b_Intercept", "b_year", "b_dah_19_in_mill"))
library(bayesplot)
color_scheme_set("red") #from bayesplot
theme_set(ggthemes::theme_gdocs())

post_ranef <- data.frame(ranef(bayes_lmm)$iso3c[,,2])
post_ranef$iso3c <- rownames(post_ranef)
post_ranef_subset <- post_ranef[post_ranef$iso3c%in%country_subset,]



ggplot(post_ranef_subset, aes(x=iso3c, y=Estimate, group=iso3c, color=iso3c)) + 
  geom_point(size=3) + 
  geom_hline(yintercept=0, color="red") + 
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5), size=1.25)


