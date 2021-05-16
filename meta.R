# R CMD javareconf

# install.packages('rJava')
# install.packages('tidyverse')
# install.packages('metafor')


library(tidyverse)
library(dplyr)
library(mixmeta)

data <- read_csv('./data.csv')


gnrha <- filter(data, 
                 !is.na(z), 
                 treatment == 'GnRHa',
                 !(study == 'Schagen' & table == 3))
                 # !(study == 'Schagen' & table == 2),
                 # !(study == 'Schagen' & table == 4))


gnrha <- gnrha %>% mutate(measure = case_when(measure == 'HAZ_spine_BMD' ~ "spine_BMD",
                                              measure == 'HAZ_hip_BMD' ~ "hip_BMD",
                                              measure == 'right_hip_BMD' ~ "hip_BMD",
                                              measure == 'left_hip_BMD' ~ "hip_BMD",
                                              TRUE ~ measure))




gnrha <- gnrha %>% mutate(measure = factor(measure))
gnrha <- gnrha %>% mutate(sex = factor(sex))

gnrha_treat <- filter(gnrha, month != 0)

gnrha_m1 <- mixmeta(z ~ month + sex, SE, data=gnrha, method="ml", random = ~ 1 | study / cohort)
# model2 <- mixmeta(z ~ month + sex + age_0, SE, data=gnrha_treat, method="ml", random = ~ 1 | study / cohort)
# model3 <- mixmeta(z ~ month + age_0, SE, data=gnrha_treat, method="ml", random = ~ 1 | study / cohort)

summary(gnrha_m1)
# summary(model2)
# summary(model3)

testosterone <- filter(data, !is.na(z), treatment == 'Testosterone')
estrogen <- filter(data, !is.na(z), treatment == 'Estrogen')

# testosterone %>% mutate(sex = factor(testosterone$sex))


t_m1 <- mixmeta(z ~ month, SE, data=testosterone, method="ml", random = ~ 1 | study / cohort)
e_m1 <- mixmeta(z ~ month, SE, data=estrogen, method="ml", random = ~ 1 | study / cohort)
#
# summary(t_m1)
# summary(e_m1)
#
male_df <- data.frame(month = 0:36, sex=factor('M'))
female_df <- data.frame(month = 0:36, sex=factor('F'))


male_df$gnrha_z  <- predict(gnrha_m1, male_df)
male_df$e_z <- predict(e_m1, data.frame(month = 0:36))



female_df$gnrha_z <- predict(gnrha_m1, female_df)
female_df$t_z <- predict(t_m1, data.frame(month = 0:36))

# male_df
# female_df 

measures <- unique(gnrha$measure)
for (m in measures) {
  measure_df  <- filter(gnrha, measure == m)
  sex <- measure_df$sex[!is.na(measure_df$sex)]

  if ("M" %in% sex && "F" %in% sex) {
    measure_model <- mixmeta(z ~ month + sex, SE, data=measure_df, method="ml", random = ~ 1 | study / cohort)
    # print(m)
    # print(summary(measure_model))
  } else {
    measure_model <- mixmeta(z ~ month, SE, data=measure_df, method="ml", random = ~ 1 | study / cohort)
    # print(m)
    # print(summary(measure_model))
  }
  
}



