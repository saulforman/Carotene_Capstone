setwd('/Users/saulforman/Desktop/capstone')
data = read.csv('emory_resume_data.csv')
head(data)
library(dplyr)

## Rename columns
colnames(data) = c('Resume', 'Carotene', 'Skill')

## Randomly sample carotenes to use for Gibbs Sampling
carotenes = data %>%
  select(Carotene) %>%
  distinct() %>%
  sample_n(20, replace = FALSE)

data_subset = filter(data, Carotene %in% carotenes$Carotene) 

data_subset = data_subset %>%
  arrange(desc(Carotene), desc(Resume))

data_final$tfidf = data_final$tf * data_final$idf
## Get number of EACH skill in carotene
## Note: Need distinct() because multiples in posting
skill_in_carotene = data_subset %>%
  distinct() %>%
  select(-Resume) %>%
  group_by(Carotene, Skill) %>%
  tally()

## TF
df = data_subset %>%
  group_by(Skill) %>%
  tally() %>%
  mutate(idf=1/n) %>%
  select(-n) %>%
  right_join(skill_in_carotene, by=c('Skill'))
## IDF
tf = data_subset %>%
  group_by(Carotene, Skill) %>%
  tally() %>%
  right_join(df, by=c('Carotene', 'Skill')) %>%
  rename(tf=n.x, n=n.y)

## Get apps-per-carotene
apps_per_carotene = data_subset %>%
  select(-Skill) %>%
  distinct() %>%
  group_by(Carotene) %>%
  tally()
## Get skills-per-resume
skills_per_job = data_subset %>%
  select(-Carotene) %>%
  distinct() %>%
  group_by(Resume) %>%
  tally()
## Apply to tf dataframe
data_final = filter(tf, Carotene %in% carotenes$Carotene) %>%
  left_join(apps_per_carotene, by=c('Carotene')) %>%
  rename(n=n.x, n_apps=n.y)

# Gibbs Sampling
library(rjags)

mod_string = " model {

  for (a in 1:length(Apps_Per_Carotene)) {
    Apps_Per_Carotene[a] ~ dpois(lam)
  }

  lam ~ dgamma(0.01, 0.01)

  for (b in 1:length(N_Skill_in_Carotene)) {
    N_Skill_in_Carotene[b] ~ dbinom(p[b], Apps_Per_Carotene[b])
    logit(p[b]) = alpha + beta[1]*IDF[b] + beta[2]*TF[b]*IDF[b]
  }

  alpha ~ dnorm(0.0, 1.0/1e6)
  for (c in 1:2) {
    beta[c] ~ dexp(1.0)
  }

} "


data_jags = list(Apps_Per_Carotene=data_final$n_apps,
                 N_Skill_in_Carotene=data_final$n,
                 TF=data_final$tf,
                 IDF=data_final$idf)

params = c('lam','alpha','beta')

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod, 3e3)
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1e4)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

par(mar=c(2,2,2,2))
plot(mod_sim, ask = TRUE)

dic = dic.samples(mod, n.iter=1e3)

## Model checking
pm_params = colMeans(mod_csim)

summary(mod_sim)



## Simulate
head(mod_csim)
num_apps = rpois(1000, mod_csim[,4])
logit_p = mod_csim[,1] + mod_csim[1:1000,3]*tf[1:1000,]$tf*tf[1:1000,]$idf
p = exp(logit_p)/(1+exp(logit_p))
num_skills = rbinom(1000, num_apps, p)

num_skill = rbinom()