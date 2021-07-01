library(PASWR2)
library(tidyverse)
library(dplyr)
library(BSDA)

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


tTest  <- function (meanx, sdx, nx, meany, sdy, ny, alternative) {
  res  <- tsum.test(meanx, s.x = sdx, n.x = nx, mean.y = meany, s.y = sdy, n.y = ny, alternative = alternative)
  res$p.value
}

tTestV <- Vectorize(tTest, vectorize.args = c('meanx', 'sdx', 'nx', 'meany', 'sdy', 'ny'))

tTestF  <- function (meanx, sdx, nx) {
  res  <- tsum.test(meanx, s.x = sdx, n.x = nx, mean.y = 0, s.y = 1, n.y = 203, alternative = "two.sided")
  res$p.value
}

tTestF <- Vectorize(tTestF, vectorize.args = c('meanx', 'sdx', 'nx'))


tTestM  <- function (meanx, sdx, nx) {
  res  <- tsum.test(meanx, s.x = sdx, n.x = nx, mean.y = 0, s.y = 1, n.y = 239, alternative = "two.sided")
  res$p.value
}

tTestM <- Vectorize(tTestM, vectorize.args = c('meanx', 'sdx', 'nx'))


# Load data
data <- read_csv('./data.csv')


# Prepare Testosterone data.frames
testosterone <- filter(data, !is.na(z), treatment == 'Testosterone')
t_start  <- filter(testosterone, month == 0)
# t_start  <- filter(t_start, id  != "21a")
# t_start  <- filter(t_start, id  != "22a")
# t_start  <- filter(t_start, id  != "24b")

t_followup  <- filter(testosterone, month >= 17)
# t_start$id
# t_followup$id

t_followup$start_z  <- t_start$z
t_followup$start_SD  <- t_start$SD

# t_followup$z
# t_followup$start_z

print('testosterone')
t_followup  <- t_followup %>% mutate(z_vs_start = tTestV(z, SD, n, start_z, start_SD, n, 'greater'))
t_followup  <- t_followup %>% mutate(z_vs_baseline = tTestV(z, SD, n, baseline, baseline_SD, n, "less"))
t_followup  <- t_followup %>% mutate(z_vs_reference = tTestF(z, SD, n))
# t_followup$id
# t_followup$z
# t_followup$z_vs_baseline


write_csv(t_followup, file = "./t_followup.csv")

print('estrogen')
# Prepare Estrogen data.frames
estrogen <- filter(data, !is.na(z), treatment == 'Estrogen')
e_start  <- filter(estrogen, month == 0)
# e_start  <- filter(e_start, id != "17b")
# e_start  <- filter(e_start, id != "18b")
e_followup  <- filter(estrogen, month >= 21)

e_followup$z
e_followup$id

e_followup$start_z  <- e_start$z
e_followup$start_SD  <- e_start$SD

# e_followup$id
# e_start$id


e_followup  <- e_followup %>% mutate(z_vs_start = tTestV(z, SD, n, start_z, start_SD, n, "greater"))
e_followup  <- e_followup %>% mutate(z_vs_baseline = tTestV(z, SD, n, baseline, baseline_SD, n, "less"))
e_followup  <- e_followup %>% mutate(z_vs_reference = tTestM(z, SD, n))
# e_followup$id
# e_followup$z_vs_baseline
# e_followup$z_vs_start

write_csv(e_followup, file = "./e_followup.csv")


# e_followup  <- e_followup %>% mutate(z_p = z_test_M(z, SD, n))
# e_followup$z_p
#



