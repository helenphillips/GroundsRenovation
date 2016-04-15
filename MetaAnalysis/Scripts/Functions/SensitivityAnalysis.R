Sensitivity_Analysis <- function(model, reps){
	
	newdat <- expand.grid(predictor = levels(model@frame[,2]), response= 0)
	names(newdat)[2] <- names(model@frame)[1]
	names(newdat)[1] <- names(model@frame)[2]
	
	newdat[2] <- predict(model,newdat,re.form=NA)
	mm <- model.matrix(terms(model),newdat)
	pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
	# tvar1 <- pvar1+VarCorr(density3)$Study.ID[1]  ## must be adapted for more complex models
	newdat$se = sqrt(pvar1) ## Fixed effects uncertanty only

	means <- newdat
	means$Habitat <- tolower(means$Habitat)

	####### 5. Other habitats
	shrubs <- read.csv("Papers/1979_Strong/Data.csv")
	trees <- shrubs$Species.Richness[shrubs$Host == "Trees" & shrubs$Status == "Middle"]
	introduced_shrubs <- shrubs$Species.Richness[shrubs$Host == "Shrubs" & shrubs$Status == "Middle"]
	introduced_shrubs_coef <- introduced_shrubs/trees
	angiosperm_shrubs <- shrubs$Species.Richness[shrubs$Host == "Shrubs" & shrubs$Status == "Middle"]
	angiosperm_shrubs_coef <- angiosperm_shrubs/trees

	scriven <- garden[garden$Study.ID == "2013_Scriven 1",]
	species_poor_hedge <- scriven$Taxon.Richness[scriven$Habitat == "species-poor hedgerow"]
	species_rich_hedge <- scriven$Taxon.Richness[scriven$Habitat == "species-rich hedgerow"]
	species_poor_hedge_coef <- species_poor_hedge/species_rich_hedge

	
	habitat_areas <- data.frame(
	Habitat = c("Broadleaved woodland", "Acid grassland (heath)", "Chalk grassland", "Neutral grassland", "Fen (incl. reedbed)", "Marginal vegetation (pond edge)", "Ponds", "Green roof", "Species-poor hedgerow", "Species-rich hedgerow", "Short/perennial vegetation", "Amenity grass/turf", "Introduced shrubs", "Hard standing", "Fern and cycad planting", "Agricultural plants", "Paleogene Asteraceae", "Neogene grass", "Cretaceous Angiosperm shrubs", "Total"),
	Current_area_m2 = c(1978, 100, 425, 2050, 75, 190, 339, 9, 109, 77, 373.9, 3657, 2000, 9506, 0, 0, 0, 0, 0, NA),
	Proposed_area_m2=c(3267, 82, 526, 2141, 134, 122, 460, 83, 0, 159, 736, 518, 1049, 9076, 760, 570, 177, 157, 245,NA)
		)
	habitat_areas$Habitat <- tolower(habitat_areas$Habitat)
	habitat_areas <- merge(habitat_areas, means, by.x = "Habitat", by.y = "Habitat", all.x = TRUE)
	names(habitat_areas)[4] <- "SpeciesDensity_10m2"

	habitat_areas$SpeciesDensity_10m2 <- exp(habitat_areas$SpeciesDensity_10m2)
	habitat_areas$se <- exp(habitat_areas$se)
		
	habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "fern and cycad planting"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "introduced shrubs"] 
	habitat_areas$se[habitat_areas$Habitat == "fern and cycad planting"] <- 
			habitat_areas$se[habitat_areas$Habitat == "introduced shrubs"] * 1.5
						
	habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "cretaceous angiosperm shrubs"] <- 
			habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "introduced shrubs"]
	habitat_areas$se[habitat_areas$Habitat == "cretaceous angiosperm shrubs"] <- 
			habitat_areas$se[habitat_areas$Habitat == "introduced shrubs"] * 1.5
	
	habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "species-poor hedgerow"] <- 
			habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "species-rich hedgerow"] * species_poor_hedge_coef
	habitat_areas$se[habitat_areas$Habitat == "species-poor hedgerow"] <- 
			habitat_areas$se[habitat_areas$Habitat == "species-rich hedgerow"] * 1.5
			
	habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "neogene grass"] <- 
			habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "amenity grass/turf"]
	habitat_areas$se[habitat_areas$Habitat == "neogene grass"] <- 
			habitat_areas$se[habitat_areas$Habitat == "amenity grass/turf"] * 1.5
			
	habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "paleogene asteraceae"] <- 
			habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "short/perennial vegetation"]
	habitat_areas$se[habitat_areas$Habitat == "paleogene asteraceae"] <- 
			habitat_areas$se[habitat_areas$Habitat == "short/perennial vegetation"] * 1.5
			
	habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "hard standing"] <- 0
	habitat_areas$se[habitat_areas$Habitat == "hard standing"] <- 0


	### Area weights
	totalCurrent <- sum(habitat_areas$Current_area_m2[1:19], na.rm = TRUE)
	totalProposed <- sum(habitat_areas$Proposed_area_m2[1:19], na.rm = TRUE)
	Current_weight <- habitat_areas$Current_area_m2/totalCurrent
	Current_weight <- Current_weight[-length(Current_weight)]
	Proposed_weight <- habitat_areas$Proposed_area_m2/totalProposed
	Proposed_weight <- Proposed_weight[-length(Proposed_weight)]

	changes <- rep(NA, length=reps)

	for(turn in 1:reps){
		temp_df <- habitat_areas[-nrow(habitat_areas),]
		
		temp_df$SpeciesDensity_10m2 <- rnorm(temp_df$SpeciesDensity_10m2, temp_df$SpeciesDensity_10m2, temp_df$se)
		
		temp_df$Corrected_SpeciesDensity_Current <- calculate_density_cSAR(S= temp_df$SpeciesDensity_10m2, A=10, z=0.1, newA= temp_df$Current_area_m2)
		temp_df$Corrected_SpeciesDensity_Proposed <- calculate_density_cSAR(S= temp_df$SpeciesDensity_10m2, A=10, z=0.1, newA= temp_df$Proposed_area_m2)
		
		if(any(is.na(temp_df$Corrected_SpeciesDensity_Current))){ temp_df$Corrected_SpeciesDensity_Current[is.na(temp_df$Corrected_SpeciesDensity_Current)] <- 0}
		if(any(is.na(temp_df$Corrected_SpeciesDensity_Proposed))){ temp_df$Corrected_SpeciesDensity_Proposed[is.na(temp_df$Corrected_SpeciesDensity_Proposed)] <- 0}
		
		
				
		Current_Weighted_density <- temp_df$SpeciesDensity_10m2 * Current_weight
		temp_df$Current_Weighted_Correcteddensity <- temp_df$Corrected_SpeciesDensity_Current * Current_weight

		Proposed_Weighted_density <- temp_df$SpeciesDensity_10m2 * Proposed_weight
		temp_df$Proposed_Weighted_Correcteddensity <- temp_df$Corrected_SpeciesDensity_Proposed * Proposed_weight

		current_total <- sum(temp_df$Current_Weighted_Correcteddensity)
		proposed_total <- sum(temp_df$Proposed_Weighted_Correcteddensity)
		diff <-  proposed_total - current_total

		percent_change <- (diff/current_total) * 100
		changes[turn] <- percent_change
	}
	return(changes)
}