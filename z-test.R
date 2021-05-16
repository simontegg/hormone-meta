library(PASWR2)
library(tidyverse)
library(dplyr)

# sink('./z-test.txt')

z_test_F  <- function (mean, s, n) {
  res  <- zsum.test(0, sigma.x = 1, n.x = 203, mean, sigma.y = s, n.y = n, alternative = "greater")
  res$p
}

z_test_M  <- function (mean, s, n) {
  res  <- zsum.test(0, sigma.x = 1, n.x = 239, mean, sigma.y = s, n.y = n, alternative = "greater")
  res$p
}

z_test_F <- Vectorize(z_test_F, vectorize.args = c('mean', 's', 'n'))
z_test_M <- Vectorize(z_test_M, vectorize.args = c('mean', 's', 'n'))


# Load data
data <- read_csv('./data.csv')


# Prepare Testosterone data.frames
testosterone <- filter(data, !is.na(z), treatment == 'Testosterone')
t_followup  <- filter(testosterone, month >= 24)


t_followup  <- t_followup %>% mutate(z_p = z_test_F(z, SD, n))
t_followup$z_p




# Prepare Estrogen data.frames
estrogen <- filter(data, !is.na(z), treatment == 'Estrogen')
e_followup  <- filter(estrogen, month >= 24)
e_followup  <- e_followup %>% mutate(z_p = z_test_M(z, SD, n))
e_followup$z_p




