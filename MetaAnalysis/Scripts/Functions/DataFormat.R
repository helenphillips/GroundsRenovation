convertArea <- function(column, column_units, type = c("sample", "habitat")){
	new_areas <- column

	if(type == 'habitat'){
		cat("Converting habitat area to hectares...\n")
		new_areas <- ifelse(column_units[!is.na(column_units)] == "m2", new_areas* 0.0001, new_areas)
		cat("Currently only converts m2...stop being lazy and write more code \n")
		}
	
	if(type == "sample"){
		cat("Converting sample area to m2..\n")
		new_areas <- ifelse(column_units[!is.na(column_units)] == "cm3", new_areas*1e-6, new_areas)
		cat('Currently onlu converts cm3 to m3....stop being lazy and write more code\n')
	}
	return(new_areas)
}