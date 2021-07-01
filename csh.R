library(tidyverse)
library(dplyr)
library(mixmeta)
library(mixmeta)

sink('./csh.txt')

# Load data
data <- read_csv('./data.csv')
data <- data %>% mutate(measure = case_when(measure == 'HAZ_spine_BMD' ~ "spine_BMD",
                                            measure == 'HAZ_hip_BMD' ~ "hip_BMD",
                                            measure == 'right_hip_BMD' ~ "hip_BMD",
                                            measure == 'left_hip_BMD' ~ "hip_BMD",
                                            TRUE ~ measure))


# Prepare Testosterone data.frames
testosterone <- filter(data, !is.na(z), treatment == 'Testosterone')
t_start  <- filter(testosterone, month == 0)
t_followup  <- filter(testosterone, month >= 17)

t_vs_baseline  <- bind_rows(t_followup, t_followup, .id = 'ID')
t_vs_baseline  <- t_vs_baseline %>% mutate(z = case_when(ID == "2" ~ baseline,
                                                         TRUE ~ z),
                                          SE = case_when(ID == "2" ~ baseline_SE,
                                                         TRUE ~ SE),
                                      period = case_when(ID == "2" ~ "baseline",
                                                         TRUE ~ "followup"))

t_vs_baseline  <- t_vs_baseline %>% mutate(period = factor(period))

t_vs_start  <- bind_rows(t_start, t_followup)
t_vs_start  <- t_vs_start %>% mutate(period = case_when(month == 0 ~ "start",
                                                        TRUE ~ 'followup'))
t_vs_start  <- t_vs_start %>% mutate(period = factor(period))


# Prepare Estrogen data.frames
estrogen <- filter(data, !is.na(z), treatment == 'Estrogen')
e_start  <- filter(estrogen, month == 0)
e_followup  <- filter(estrogen, month >= 21)

e_vs_baseline  <- bind_rows(e_followup, e_followup, .id = 'ID')
e_vs_baseline  <- e_vs_baseline %>% mutate(z = case_when(ID == "2" ~ baseline,
                                               TRUE ~ z),
                                SE = case_when(ID == "2" ~ baseline_SE,
                                               TRUE ~ SE),
                            period = case_when(ID == "2" ~ "baseline",
                                               TRUE ~ "followup"))
e_vs_baseline  <- e_vs_baseline %>% mutate(period = factor(period))

e_vs_start  <- bind_rows(e_start, e_followup)
e_vs_start  <- e_vs_start %>% mutate(period = case_when(month == 0 ~ "start",
                                                        TRUE ~ 'followup'))
e_vs_start  <- e_vs_start %>% mutate(period = factor(period))


# Run models
t_m1  <- mixmeta(z ~ period, SE, data=t_vs_baseline, method="ml", random = ~ 1 | study / cohort) 
t_m2  <- mixmeta(z ~ period, SE, data=t_vs_start, method="ml", random = ~ 1 | study / cohort) 
e_m1  <- mixmeta(z ~ period, SE, data=e_vs_baseline, method="ml", random = ~ 1 | study / cohort)
e_m2  <- mixmeta(z ~ period, SE, data=e_vs_start, method="ml", random = ~ 1 | study / cohort)


# Report
summary(t_m1)
summary(t_m2)
summary(e_m1)
summary(e_m2)


predict_df <- data.frame(period = c('start', 'followup'))
predict_df$t_m2  <- predict(t_m2, predict_df)
predict_df$e_m2  <- predict(e_m2, predict_df)
write_csv(predict_df, file = "./predict_csh.csv")


# summary(e_m1)


measures <- unique(testosterone$measure)
for (m in measures) {
  measure_df  <- filter(testosterone, measure == m)
  sex <- measure_df$sex[!is.na(measure_df$sex)]
  studies  <- unique(measure_df$study)


  if (length(studies) > 1) {
    # measure_model <- mixmeta(z ~ period, SE, data=measure_df, method="ml", random = ~ 1 | study / cohort)

    # print(m)
    # print(summary(measure_model))
  } else {
    # measure_model <- mixmeta(z ~ period, SE, data=measure_df, method="ml")
    # print(m)
    # print(summary(measure_model))
  }
#
  # print(measure_df$absolute)
  # print(measure_df$abs_SE)
  # print(measure_df$sex)
  # print(measure_df$study)
  # print(measure_df$month)
  # print(measure_df$cohort)
}



