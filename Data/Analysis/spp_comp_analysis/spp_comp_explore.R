# explore the species composition data

library(tidyverse)

## open data
spp_comp = read.csv('../../Spp_comp/species_comp_withinfo.csv')

## most dominant species
spp_comp_group_by_species = group_by(spp_comp, binomial)
spp_comp_sum = summarise(spp_comp_group_by_species, sum = sum(Percent.Cover))
spp_comp_sum_arr = arrange(spp_comp_sum, sum)
sum(spp_comp_sum_arr$sum[c(19, 18, 16, 9)])/sum(spp_comp_sum_arr$sum[1:19])
