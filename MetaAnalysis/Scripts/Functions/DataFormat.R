convertArea <- function(column, column_units, type = c("sample", "habitat")){
	new_areas <- column

	if(type == 'habitat'){
		cat("Converting habitat area to m2...\n")
		new_areas <- ifelse(column_units == "ha", new_areas* 10000, new_areas)
		cat("Currently only converts m2...stop being lazy and write more code \n")
		}
	
	if(type == "sample"){
		cat("Converting sample area to m2..\n")
		new_areas <- ifelse(column_units == "cm2", new_areas*0.0001, new_areas)
	}
	return(new_areas)
}




calculate_density_cSAR <- function(S=garden$Taxon.Richness, A=garden$Sampled.Area_metres, x=0.1, newA = 10){
	log_c <- log(S)-(log(A)*x)
	log_new_S <- log_c + x*log(newA)
	new_S <- exp(log_new_S)
	return(new_S)
}
