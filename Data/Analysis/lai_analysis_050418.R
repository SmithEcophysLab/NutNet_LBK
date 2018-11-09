library(dplyr)
library(magrittr)

# load computed lai values
lai = read.table('/Users/nicksmith/Documents/Research/NutNet/Rangeland/Data/LAI/Rangeland_LAI_050418', header = T)

# load raw light data and combine
lai_raw = read.table('/Users/nicksmith/Documents/Research/NutNet/Rangeland/Data/LAI/RL050418_edited copy.txt', header = F)
colnames(lai_raw)[2] = 'B_Obs'
colnames(lai_raw)[11] = 'Plot'
lai_plus = left_join(lai, lai_raw, by = 'B_Obs')

# aggregate
lai_group = group_by(lai_plus, Plot)
lai_mean = summarise(lai_group, lai_mean = mean(LAI), lai_sd = sd(LAI))

hist(lai_mean$lai_mean)



