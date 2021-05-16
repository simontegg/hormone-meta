# R CMD javareconf

# install.packages('rJava')
# install.packages('tidyverse')
# install.packages('metafor')


library(tidyverse)
library(dplyr)
library(mixmeta)

data <- read_csv('./data.csv')


gnrha <- filter(data, 
                 !is.na(absolute), 
                 treatment == 'GnRHa',
                 !(study == 'Schagen' & table == 3))
                 # !(study == 'Schagen' & table == 2),
                 # !(study == 'Schagen' & table == 4))


gnrha <- gnrha %>% mutate(measure = case_when(measure == 'HAZ_spine_BMD' ~ "spine_BMD",
                                              measure == 'HAZ_hip_BMD' ~ "hip_BMD",
                                              measure == 'right_hip_BMD' ~ "hip_BMD",
                                              measure == 'left_hip_BMD' ~ "hip_BMD",
                                              TRUE ~ measure))


male_df <- data.frame(month = 0:36, sex=factor('M'))
female_df <- data.frame(month = 0:36, sex=factor('F'))


# male_df$gnrha_z  <- predict(gnrha_m1, male_df)
# male_df$e_z <- predict(e_m1, data.frame(month = 0:36))
#
# female_df$gnrha_z <- predict(gnrha_m1, female_df)
# female_df$t_z <- predict(t_m1, data.frame(month = 0:36))

# male_df
# female_df 

measures <- unique(gnrha$measure)
for (m in measures) {
  measure_df  <- filter(gnrha, measure == m)
  sex <- measure_df$sex[!is.na(measure_df$sex)]
  studies  <- unique(measure_df$study)

  print(studies)

  print(measure_df$absolute)
  print(measure_df$abs_SE)
  print(measure_df$sex)
  print(measure_df$study)
  print(measure_df$month)
  print(measure_df$cohort)


  # if (length(studies) > 1) {
    if ("M" %in% sex && "F" %in% sex) {
      print(m)

      measure_model <- mixmeta(absolute ~ month, abs_SE, data=measure_df, method="ml", random = ~ 1 | study / cohort)
      print(summary(measure_model))
    } else {
      measure_model <- mixmeta(absolute ~ month, abs_SE, data=measure_df, method="ml", random = ~ 1 | study / cohort)
      print(m)
      print(summary(measure_model))
    }
  # }
}



