sc = read.csv('/Users/nicksmith/Documents/Git/NutNet_LBK/Data/SPP_comp/species_comp.csv')
sp = read.csv('/Users/nicksmith/Documents/Git/NutNet_LBK/Data/SPP_comp/species.csv')

sc$binomial = 'NA'
sc$lifeform = 'NA'
sc$family = 'NA'

for (i in 1:length(sc$binomial)){
	
	sc$binomial[i] <- as.character(subset(sp, Code == as.character(sc$Code[i]))$Latin.binomial)
	sc$lifeform[i] <- as.character(subset(sp, Code == as.character(sc$Code[i]))$Life.form)
	sc$family[i] <- as.character(subset(sp, Code == as.character(sc$Code[i]))$Family)
	
}

sc$genus = vapply(strsplit(sc$binomial, " ", fixed = TRUE), "[", "", 1)
sc$species = vapply(strsplit(sc$binomial, " ", fixed = TRUE), "[", "", 2)

# write.csv(sc, '/Users/nicksmith/Documents/Git/NutNet_LBK/Data/SPP_comp/species_comp_withinfo.csv', row.names = F)
