#最終更新：2024年2月19日 14時25分

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

##############################
#
# ローデータ編集のための箇所 #
#
##############################

raw_metaphor_dat <- read.csv("../data/20191221_FeatExt_Metaphor_edited.csv")
raw_simile_dat <- read.csv("../data/20200113_FeatExt_Simile_edited.csv")

#除外するIDで除外
rm_metaphor_pwid_lst = c(442799, 963833)
rm_simile_pwid_lst = c(498992, 361696)

#年齢と性別を確認するためだけのデータの作成
dat_demographic_metaphor <- raw_metaphor_dat %>% 
  dplyr::filter(!P_ID %in% rm_metaphor_pwid_lst) %>%
  dplyr::select(P_ID, Sex, Age)

dat_demographic_simile <- raw_simile_dat %>% 
  dplyr::filter(!P_ID %in% rm_simile_pwid_lst) %>%
  dplyr::select(P_ID, Sex, Age)

dat_demograhic <- dat_demographic_metaphor %>%
  dplyr::bind_rows(dat_demographic_simile)

#年齢と性別を確認するためだけのデータの作成
dat_age <- dat_demograhic %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))

dat_sex <- dat_demograhic %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())

