
if(Sys.info()['nodename'] == "helensminimac.nhm.ac.uk"){
	path = "/Users/hp1111/PhD/git_phdChapters/git_WildlifeGarden"
}else{
	path = "/Users/helenphillips/PhD_git/WildlifeGarden_chapter"}

##### Re-doing functions quickly

remove_duplicates <- function(plants, columns){
	
	col <- which(names(plants) == columns)
	
	unique_areas <- unique(plants[,col])
	new_plants <- list()

	for(a in 1:length(unique_areas)){
	
		tmp <- plants[plants[,col] == unique_areas[a],]
		
		if(length(tmp$Species) != length(unique(tmp$Species))){
			
			print(paste(unique_areas[a], "is un-equal", sep =" "))
			duplicates <- tmp[duplicated(tmp$Species),]
			duplicates2 <- tmp[tmp$Species %in% duplicates$Species,]
			
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
			print(paste(unique_areas[a], "is equal", sep =" "))
			new_plants[[a]] <- tmp	
		}
		
	}
names(new_plants) <- unique_areas
return(new_plants)

}

########## Looking at data from full database

data <- read.csv("~/Box Sync/help/PhD_copy/Wildlife Garden/PlantsDatabase/DataFiles/WLG observation database 22_06_16.csv")

nrow(data) # 36588
levels(data$HigherGroup)

inverts <- c("Arachnida", "Crustacea", "Insecta", "Mollusca", "Myriapoda", "OTHER INVERTEBRATES")

inverts <- droplevels(data[data$HigherGroup %in% inverts,])
nrow(inverts) # 8363

levels(inverts$Habitat)

useable_habitats <- c("Fen", "Grassland", "Hedgerow", "Lowland Heath", "Pond", "Pond Margin", "Reed Bed","Scrub","Woodland")  

inverts <- droplevels(inverts[inverts$Habitat %in% useable_habitats,]) # 4842

inverts$Date2 <- as.Date(inverts$Date, format = "%d-%b-%y")
inverts$Year <- format(inverts$Date2, "%Y")

inverts <- droplevels(inverts[inverts$Year >= 2013,]) ##  1242

inverts$HabitatNumber <- as.factor(paste(inverts$Habitat, inverts$BedFK))


new_inverts <- remove_duplicates(inverts, columns = "HabitatNumber")
