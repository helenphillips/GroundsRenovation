check_comparisons <- function(data){
	require(lattice)
	
	sheet2 <- gs_title("Wildlife Garden - data collection") 
	sheet2 <- gs_read(sheet2, ws = "MetaData") 
	habitats <-t(as.data.frame(sheet2[8, 4:23]))
	
	grounds_landuses <- data.frame(NHMGrounds=c(
		as.vector(habitats[,1])
		),
		UKBAP = c(
		"Broadleaved, Mixed and Yew Woodland/Tradition Orchards/Parkland",
		"Acid Grassland",
		"Calcareous Grassland",
		"Neutral Grassland/Meadows Improved Grassland",
		"Fen, Marsh and Swamp",
		"Boundary and Linear Features/Hedgerows",	
		"Standing Open Waters and Canals/Ponds",
		NA,
		"Boundary and Linear Features/Hedgerows",
		"Boundary and Linear Features/Hedgerows",
		NA,
		NA,
		NA,
		NA,
		NA,
		NA,
		"Broadleaved, Mixed and Yew Woodland/Tradition Orchards/Parkland",	
		NA,
		NA,
		NA))
	
	all_landuses <- grounds_landuses[,1]
	
	acquired_landuses <- length(levels(data$Habitat))
	
	cat(paste("Of the", length(all_landuses), "habitats only", acquired_landuses, "currently have data\n", sep = " "))
	print(levels(data$Habitat))
	
	cat(paste("Currently there are", length(unique(data$Study.ID)), "studies in the dataset\n", sep=" "))
	
	## How many studies have different land uses
	
	## Variation in habitat area
	variation(data, "Habitat.Area")
	## Variation in sampling area
	variation(data, "Sampled.Area")

	## Time series data
	variation(data, "Habitat.Age")
	
	cat(paste("Looking at within study habitat comparisons....\n"))
	hab_comparison <- habitat_comparison(data, all_landuses)
	#hab_comparison <- ifelse(hab_comparison == 0, NA, hab_comparison)
	p <- levelplot(hab_comparison, at = seq(1, max(hab_comparison), 1),col.regions = colorRampPalette(c("grey", "black")), scale=list(x=list(rot=45)), xlab = "", ylab = "")
	
	return(list(hab_comparison, p))
}
	
	
variation <- function(data, Column){ ## Function for checking for variation in time or area
	
	col <- which(names(data) == Column)
	## Variation in habitat area
	variation <- as.data.frame(aggregate(data[,col], list(data$Habitat, data$Study.ID), function(x){variation = var(x, na.rm=TRUE)}))
	study_data <- variation[complete.cases(variation),]
	study_data <- study_data[study_data$x > 0,]
	
	n_studies <- unique(study_data$Group.2)
	cat(paste(length(n_studies),"studies have variation in", Column, "\n", sep=" "))
	print(as.vector(n_studies))
	n_habitats <- unique(study_data$Group.1)
	cat(paste(length(n_habitats),"habitats have variation in", Column, "\n", sep=" "))
	print(as.vector(n_habitats))
}


habitat_comparison <- function(dat, all_landuses){
	
	hab_comp <- matrix(data = 0, nrow = length(all_landuses), ncol = length(all_landuses))
	colnames(hab_comp) <- rownames(hab_comp) <- all_landuses
	
	studies <- levels(dat$Study.ID)
	for(study in 1:length(studies)){
		#print(studies[study])
		tmp <- droplevels(dat[dat$Study.ID == studies[study],])
		habitats <- levels(tmp$Habitat)
		#print(habitats)
		for(habs in 1:length(habitats)){
			#print(habitats[habs])
			# Check that more than one land use in the habitat, add one to that comparison if it does	
			for(other_habs in 1:length(habitats)){
				#print(paste("Comparing", habitats[habs], "with", habitats[other_habs]))
				
				if(other_habs == habs){
					if(sum(tmp$Habitat == habitats[habs]) > 1){
					#print(paste("More than one site in", habitats[habs]))
					hab_comp[which(rownames(hab_comp) == habitats[habs]), which(colnames(hab_comp) == habitats[habs])] <- 
														hab_comp[which(rownames(hab_comp) == habitats[habs]), which(colnames(hab_comp) == habitats[habs])] + 1
					}else{}#print(paste("Only one site in", habitats[habs]))}
				}else{
				
				hab_comp[which(rownames(hab_comp) == habitats[habs]), which(colnames(hab_comp) == habitats[other_habs])] <- 
								hab_comp[which(rownames(hab_comp) == habitats[habs]), which(colnames(hab_comp) == habitats[other_habs])] + 1
				}
			}
		}
	}
	return(hab_comp)
}