# R CMD javareconf

# install.packages('rJava')
# install.packages('tidyverse')
# install.packages('metafor')


library(tidyverse)
library(dplyr)
library(mixmeta)

sink('./meta.txt')


data <- read_csv('./data.csv')


base <- filter(data, 
                 treatment == 'GnRHa',
                 !(study == 'Schagen' & table == 3),
                 measure != 'HAZ_spine_BMD',
                 measure != 'HAZ_hip_BMD')
                 # !(study == 'Schagen' & table == 2),
                 # !(study == 'Schagen' & table == 4))


base <- base %>% mutate(measure = case_when(measure == 'HAZ_spine_BMD' ~ "spine_BMD",
                                              measure == 'HAZ_hip_BMD' ~ "hip_BMD",
                                              measure == 'right_hip_BMD' ~ "hip_BMD",
                                              measure == 'left_hip_BMD' ~ "hip_BMD",
                                              TRUE ~ measure))




base  <- base %>% mutate(period = case_when(month == 0 ~ "baseline",
                                              TRUE ~ "followup"))

gnrha_z <- filter(base, !is.na(z)) 
# Remove intermediate followups
gnrha_z  <- filter(gnrha_z, !(study == 'Joseph' & month == 18), !(study == 'Joseph' & month == 17))


# followups only
gnrha_treat  <- filter(gnrha_z, month != 0)
# gnrha_treat_M  <- filter(gnrha_treat, sex == 'M')
# gnrha_treat_F  <- filter(gnrha_treat, sex == 'F')



# gnrha_m1 <- mixmeta(z ~ factor(period) + factor(sex), SE, data=gnrha_z, method="ml", random = ~ 1 | study / cohort)
gnrha_m1 <- mixmeta(z ~ factor(period), SE, data=gnrha_z, method="ml", random = ~ 1 | study / cohort)

gnrha_m2 <- mixmeta(z ~ month + factor(sex), SE, data=gnrha_treat, method="ml", random = ~ I(month - 12) | study / cohort)
gnrha_m3 <- mixmeta(z ~ month, SE, data=gnrha_treat, method="ml", random = ~ I(month - 12) | study / cohort)
# gnrha_m3 <- mixmeta(z ~ month, SE, data=gnrha_treat_F, method="ml", random = ~ I(month - 4) | study / cohort)

# model2 <- mixmeta(z ~ month + sex + age_0, SE, data=gnrha_treat, method="ml", random = ~ 1 | study / cohort)
# model3 <- mixmeta(z ~ month + age_0, SE, data=gnrha_treat, method="ml", random = ~ 1 | study / cohort)

summary(gnrha_m1)
summary(gnrha_m2)
summary(gnrha_m3)

# predict_df <- data.frame(period = c('baseline', 'followup'))
# predict_df$m1  <- predict(gnrha_m1, predict_df)
# write_csv(predict_df, file = "./gnrha_m1.csv")



gnrha_abs <- filter(base, !is.na(abs_per))
# gnrha_abs_treat <- filter(gnrha_abs, month != 0)

# gnrha_abs$abs_per_SE

# gnrha_m4 <- mixmeta(abs_per ~ factor(period) + factor(sex), abs_per_SE, data=gnrha_abs, method="ml", random = ~ 1 | study / cohort)
# gnrha_m5 <- mixmeta(abs_per ~ month + factor(sex), SE, data=gnrha_treat, method="ml", random = ~ I(month - 12) | study / cohort)

# summary(gnrha_m4)





# male_df <- data.frame(month = 0:36, sex=factor('M'))
# female_df <- data.frame(month = 0:36, sex=factor('F'))


# male_df$gnrha_z  <- predict(gnrha_m1, male_df)
# male_df$e_z <- predict(e_m1, data.frame(month = 0:36))



# female_df$gnrha_z <- predict(gnrha_m1, female_df)
# female_df$t_z <- predict(t_m1, data.frame(month = 0:36))

# male_df
# female_df 

measures <- unique(gnrha_abs$measure)
for (m in measures) {
  measure_df  <- filter(gnrha_abs, measure == m)
  sex <- measure_df$sex[!is.na(measure_df$sex)]


  # if ("M" %in% sex && "F" %in% sex && m != 'spine_BMAD') {
  if (m != 'spine_BMAD') {
    print(m)
    print(measure_df$absolute)
    # measure_model <- mixmeta(absolute ~ factor(period), abs_SE, data=measure_df, random = ~ 1 | study / cohort)
    # print(summary(measure_model))
  } else {
    print('1 sex only')
    print(m)
    # measure_model <- mixmeta(z ~ month, SE, data=measure_df, method="ml", random = ~ 1 | study / cohort)
    # print(m)
    # print(summary(measure_model))
  }
}




