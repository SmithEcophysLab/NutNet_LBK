library(dplyr)
library(magrittr)
library(data.table)
library(emmeans)

# load computed lai values
lai = read.table('/Users/nicksmith/Documents/Research/NutNet/Data/LAI/Rangeland_LAI_061018', header = T)

# load raw light data and combine
lai_raw = fread('/Users/nicksmith/Documents/Research/NutNet/Data/LAI/RL061018_edited.txt', header = F, drop = c(11:13), data.table = F, skip = 35)
colnames(lai_raw)[2] = 'B_Obs'
colnames(lai_raw)[10] = 'Plot_raw'
lai_raw$Plot = sapply(strsplit(lai_raw$Plot, 'P'), '[', 2)
lai_plus = left_join(lai, lai_raw, by = 'B_Obs')

# aggregate
lai_group = group_by(lai_plus, Plot)
lai_mean = summarise(lai_group, lai_mean = mean(LAI), lai_sd = sd(LAI))

lai_mean$Plot[lai_mean$Plot == '1REDO'] <- 1

hist(lai_mean$lai_mean)
plot(lai_mean$lai_mean)
nrow(lai_mean)

lai_mean$Plot_num = NA
lai_mean$Plot_num = as.numeric(as.character(lai_mean$Plot))

lai_mean$block = NA
lai_mean$block[lai_mean$Plot_num < 15] = 1
lai_mean$block[lai_mean$Plot_num > 14 & lai_mean$Plot_num < 29] = 2
lai_mean$block[lai_mean$Plot_num > 28] = 3
lai_mean$blockfac = as.factor(lai_mean$block)

lai_block_lm = lm(lai_mean ~ blockfac, data = lai_mean)
anova(lai_block_lm)
lsmeans(lai_block_lm, ~blockfac)


