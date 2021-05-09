library(tidyverse)
library(dplyr)
library(metafor)
library(glmulti)

rma.glmulti <- function(formula, data, ...)
   rma(formula, SE, data=data, method="ML", ...)

data <- read_csv('./data.csv')


gnrha <- filter(data, 
                !is.na(z), 
                !is.na(sex), 
                treatment == 'GnRHa',
                !(study == 'Schagen' & table == 3))

gnrha %>% mutate(sex = factor(gnrha$sex))

gnrha_treat <- filter(gnrha, month != 0)

res <- glmulti(z ~ month + sex, data=gnrha,
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)

print(res)

top <- weightable(res)
top <- top[top$aicc <= min(top$aicc) + 2,]
top 


res_treat <- glmulti(z ~ month + sex + age_0, data=gnrha_treat,
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)

print(res_treat)

top <- weightable(res_treat)
top <- top[top$aicc <= min(top$aicc) + 2,]
top 


