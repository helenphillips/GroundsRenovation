remove_duplicates <- function(plants, columns){
	
	col <- which(names(plants) == columns)
	
	unique_areas <- unique(plants[,col])
	new_plants <- list()

	for(a in 1:length(unique_areas)){
	
		tmp <- plants[plants[,col] == unique_areas[a],]
		
		if(length(tmp$NBN.Name) != length(unique(tmp$NBN.Name))){
			
			# print(paste(unique_areas[a], "is un-equal", sep =" "))
			duplicates <- tmp[duplicated(tmp$NBN.Name),]
			duplicates2 <- tmp[tmp$NBN.TVK %in% duplicates$NBN.TVK,]
			
			tmp <- tmp[-c(which(tmp$ID %in% duplicates2$ID)),] ## Getting rid of all the duplicates
			
			ids <- unique(duplicates2$NBN.TVK)
			
			for(id in 1:length(ids)){
				x <- duplicates2[duplicates2$NBN.TVK == ids[id],]
				new.row <- x[1,]
				new.row$YearFirstRecorded <- min(x$YearFirstRecorded)
				new.row$YearLastRecorded <- max(x$YearLastRecorded)
				tmp <- rbind(tmp, new.row)
			}
			
			new_plants[[a]] <- tmp	
		
		}else{
			# print(paste(unique_areas[a], "is equal", sep =" "))
			new_plants[[a]] <- tmp	
		}
		
	}
names(new_plants) <- unique_areas
return(new_plants)

}