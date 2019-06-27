library(dplyr)
library(magrittr)
library(data.table)
library(emmeans)

# load computed lai values
lai = read.csv('../raw/RL062719_calc.csv', header = T)

# load raw light data and combine
lai_raw = read.csv('../raw/RL062719_edited_4r.csv', header = F)
colnames(lai_raw)[2] = 'B_Obs'
colnames(lai_raw)[10] = 'Plot_raw'
lai_raw$Plot = sapply(strsplit(as.character(lai_raw$Plot_raw), 'P'), '[', 2)
lai_plus = left_join(lai, lai_raw, by = 'B_Obs')

# aggregate
lai_group = group_by(lai_plus, Plot)
lai_mean = summarise(lai_group, lai_mean = mean(LAI), lai_sd = sd(LAI), lai_n = n())
lai_mean$Plot

hist(lai_mean$lai_mean)
nrow(lai_mean)

# write.csv(lai_mean, '../clean/lai_mean_RL062719.csv')