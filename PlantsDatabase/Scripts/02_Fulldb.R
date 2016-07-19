
if(Sys.info()['nodename'] == "helensminimac.nhm.ac.uk"){
	path = "/Users/hp1111/PhD/git_phdChapters/git_WildlifeGarden"
}else{
	path = "/Users/helenphillips/PhD_git/WildlifeGarden_chapter"}

source(file.path(path, "/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R"))


figure_out <- "~/Box Sync/help/PhD_Copy/Wildlife Garden/PlantsDatabase/Figures"

library(lattice)

##### Re-doing functions quickly

remove_duplicates <- function(db, columns){
	
	col <- which(names(db) == columns)
	
	unique_areas <- unique(db[,col])
	new_db <- list()

	for(a in 1:length(unique_areas)){
	
		tmp <- db[db[,col] == unique_areas[a],]
		
		if(length(tmp$trinomial) != length(unique(tmp$trinomial))){
			
			print(paste(unique_areas[a], "is un-equal", sep =" "))
			duplicates <- tmp[duplicated(tmp$trinomial),]
			duplicates2 <- tmp[tmp$trinomial %in% duplicates$trinomial,]
			
			tmp <- tmp[-c(which(tmp$trinomial %in% duplicates2$trinomial)),] ## Getting rid of all the duplicates
			
			trinomials <- unique(duplicates2$trinomial)
			
			res <- c()
			
			for(tri in 1:length(trinomials)){
				x <- duplicates2[duplicates2$trinomial == trinomials[tri],]
				new.row <- x[1,]
				new.row$YearFirstRecorded <- min(x$Year)
				new.row$YearLastRecorded <- max(x$Year)
				res <- rbind(res, new.row)
			}
			
			new_db[[a]] <- res	
		
		}else{
			print(paste(unique_areas[a], "is equal", sep =" "))
			new_db[[a]] <- tmp	
		}
		
	}
names(new_db) <- unique_areas
return(new_db)

}


species_similarity <- function(new_inverts){
	mat <- matrix(NA, nrow = length(new_inverts), ncol = length(new_inverts))
	
	for(l in 1:length(new_inverts)){
		## List we are checking
		for(a in 1:length(new_inverts)){
			## all the other lists we are checkng it against
			
			mat[l,a] <- (length(intersect(new_inverts[[l]]$trinomial, new_inverts[[a]]$trinomial))/length(unique(new_inverts[[l]]$trinomial)))*100
			
		}
			
	}
	
	colnames(mat) <- names(new_inverts)
	rownames(mat) <- names(new_inverts)

	return(mat)	
}



########## Looking at data from full database

data <- read.csv("~/Box Sync/help/PhD_copy/Wildlife Garden/PlantsDatabase/DataFiles/WLG observation database 22_06_16.csv")

beds <- read.csv("~/Box Sync/help/PhD_copy/Wildlife Garden/PlantsDatabase/DataFiles/wlg-beds.csv")


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
levels(inverts$HabitatNumber)

#### But what we actually want is the habitats that link with the analysis
levels(inverts$Habitat)

levels(inverts$Habitat)[levels(inverts$Habitat) == "Fen"] <- "Fen (incl. reedbed)"
levels(inverts$Habitat)[levels(inverts$Habitat) == "Lowland Heath"] <- "Acid grassland (heath)"
levels(inverts$Habitat)[levels(inverts$Habitat) == "Hedgerow"] <- "Species-rich hedgerow"
levels(inverts$Habitat)[levels(inverts$Habitat) == "Pond"] <- "Standing water"
levels(inverts$Habitat)[levels(inverts$Habitat) == "Pond Margin"] <- "Marginal vegetation"
levels(inverts$Habitat)[levels(inverts$Habitat) == "Scrub"] <- "Broadleaved woodland"
levels(inverts$Habitat)[levels(inverts$Habitat) == "Woodland"] <- "Broadleaved woodland"


test <- merge(inverts, beds, by.x="BedFK", by.y ="BedID", all.x = TRUE)
test <- droplevels(test)
garden <- OpenGS()
## WHen not working
## garden <- read.csv("/Users/hp1111/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Data/Data_extract_2016-06-02.csv")
nhm <- garden[garden$Study.ID == "PlantDatabaseExtract",]
### Adding in the NHM habitat classification
test <- merge(test, nhm, by.x="Code", by.y="Site.ID")[,c(1:31, 36)]
test <- droplevels(test)


test <- droplevels(test[test$Habitat.y != "hard standing",])
test <- droplevels(test[test$Habitat.y != "Unsure/Not Clear",]) # 1086

new_inverts <- remove_duplicates(test, columns = "Habitat.y")
new_inverts <- species_similarity(new_inverts)
png(file.path(figure_out, "Inverts_NHMHabitatsSimiarity.png"))
levelplot(new_inverts, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45)), ylab = "", xlab = "")
dev.off()