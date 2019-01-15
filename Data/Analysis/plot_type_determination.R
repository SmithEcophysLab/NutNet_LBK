# analysis to determine where NutNet plots are located
# whether a plot is considered a core plot is based on cluster analysis of plot light, biomass, and species diversity within each block
# plot types are then determined at random
# data is from 2018 pre-treatment sampling
# author: NG Smith

#######################################
# load libraries
#######################################
library(vegan)
# library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#######################################
# load in data
#######################################
biomass = read.csv('../Biomass/Biomass.csv')
light = read.csv('../Light/Light.csv')
comp = read.csv('../SPP_comp/species_comp.csv')

#######################################
# subset to only include September 2018 data
#######################################
biomass_18 = subset(biomass, Year == 2018 & Month == 9)
light_18 = subset(light, Year == 2018 & Month == 9)
comp_18 = subset(comp, Year == 2018 & Month == 9)

#######################################
# compute diversity from comp data
#######################################
plots = seq(1, 42, 1)
diversity_data = NULL
comp_18_plants = subset(comp_18, Code != 'BARE' & Code != 'LITT')
for (i in 1:length(plots)){
	
	tmp_cover = (subset(comp_18_plants, Plot == i)$Percent.Cover) / 100
	tmp_diversity = diversity(tmp_cover, index = 'simpson')
	plot = i
	diversity_data = rbind(c(plot, tmp_diversity), diversity_data)
		
}
diversity_df = as.data.frame(diversity_data)
colnames(diversity_df) = c('Plot', 'D')
# hist(diversity_df$D)

#######################################
# compute light availability
#######################################
plots = seq(1, 42, 1)
light_avail_data = NULL
for (i in 1:length(plots)){
	
	in_mean = mean(subset(light_18, Plot == i & Location == 'Ground')$Light)
	out_mean = subset(light_18, Plot == i & Location == 'Ambient')$Light
	avail = in_mean / out_mean
	plot = i
	light_avail_data = rbind(c(plot, avail), light_avail_data)
	
}
light_avail_df = as.data.frame(light_avail_data)
colnames(light_avail_df) = c('Plot', 'Light_avail')
# hist 

#######################################
# compute total biomass
#######################################
biomass_18_plants = subset(biomass_18, pft != 'dead')
biomass_group_by_plot = group_by(biomass_18_plants, Plot)
biomass_total = summarise(biomass_group_by_plot, weight_sum = sum(weight))

#######################################
# combine diversity, light availability, and biomass
#######################################

diversity_light = left_join(diversity_df, light_avail_df)
diversity_light_biomass = left_join(diversity_light, biomass_total)

#######################################
# calculate Euclidean distances for each plot based on diversity, light, and biomass
#######################################

dlb_arrange = arrange(diversity_light_biomass, Plot)
dlb_matrix = as.matrix(dlb_arrange[,c(2:4)])
rownames(dlb_matrix) = seq(1, 42, 1)

# find plots with greatest average Euclidean distance
distances = colMeans(as.matrix(dist(scale(dlb_matrix), method = 'euclidean', diag = T, upper = T)))
sort(distances[1:14], decreasing = T) # plots 1, 6, 7, 10
sort(distances[15:28], decreasing = T) # plots 25, 22, 19, 26
sort(distances[29:42], decreasing = T) # plots 31, 40, 38, 41

# some k-means plots
fviz_nbclust(scale(dlb_matrix), kmeans, method = "wss") # 4
k_dlb_block = kmeans(scale(dlb_matrix), centers = 7, nstart = 25)
fviz_cluster(k_dlb_block, data = scale(dlb_matrix)) # 

#######################################
# assign plot types to similar plots
#######################################

plot_types = c('Control', 'N', 'P', 'K', 'NP', 'NK', 'PK', 'NPK', 'Fence', 'NPK+Fence')

# plot_types_block1 = sample(plot_types)
# plot_types_block2 = sample(plot_types)
# plot_types_block3 = sample(plot_types)

plot_types_block1_all = c('xControl', plot_types_block1[1:4], 'xControl', 'xControl', plot_types_block1[5:6], 'xControl', plot_types_block1[7:10]) # remove 1, 6, 7, 10
plot_types_block2_all = c(plot_types_block2[1:4], 'xControl', plot_types_block2[5:6], 'xControl', plot_types_block2[7:8], 'xControl', 'xControl', plot_types_block2[9:10]) # remove 19, 22, 25, 26
plot_types_block3_all = c(plot_types_block3[1:2], 'xControl', plot_types_block3[3:8], 'xControl', plot_types_block3[9], 'xControl', 'xControl', plot_types_block3[10]) # remove 31, 38, 40, 41

plot_type_assignment = as.data.frame(cbind(seq(1, 42, 1), NA))
colnames(plot_type_assignment) = c('Plot', 'trt')
plot_type_assignment$trt = c(plot_types_block1_all, plot_types_block2_all, plot_types_block3_all)
nrow(subset(plot_type_assignment, trt == 'Control'))
nrow(subset(plot_type_assignment, trt == 'N'))
nrow(subset(plot_type_assignment, trt == 'P'))
nrow(subset(plot_type_assignment, trt == 'K'))
nrow(subset(plot_type_assignment, trt == 'NP'))
nrow(subset(plot_type_assignment, trt == 'NK'))
nrow(subset(plot_type_assignment, trt == 'PK'))
nrow(subset(plot_type_assignment, trt == 'NPK'))
nrow(subset(plot_type_assignment, trt == 'Fence'))
nrow(subset(plot_type_assignment, trt == 'NPK+Fence'))

# write.csv(plot_type_assignment, '../../plot_types/plot_types.csv')



