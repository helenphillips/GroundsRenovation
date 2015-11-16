species_similarity <- function(new_plants){
	mat <- matrix(NA, nrow = length(new_plants), ncol = length(new_plants))
	
	for(l in 1:length(new_plants)){
		## List we are checking
		for(a in 1:length(new_plants)){
			## all the other lists we are checkng it against
			
			mat[l,a] <- length(intersect(new_plants[[l]]$NBN.Name, new_plants[[a]]$NBN.Name))
			
		}
			
	}
	
	colnames(mat) <- names(new_plants)
	rownames(mat) <- names(new_plants)

	return(mat)	
}