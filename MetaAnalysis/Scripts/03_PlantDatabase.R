source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/RemoveDuplicates.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/SpeciesSimilarity.R")



##############
## DATA
##############


plants <- read.csv("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/DataFiles/PlantsByBed_NBN_Combined_2015-11-05-CMTR.csv")
plants$X <- NULL
nrow(plants) # 5731

plants <- plants[plants$NBN.TVK != "",]
nrow(plants) #5459 

names(plants)[1:2] <- c("ID", "Habitat")

levels(plants$Code)[levels(plants$Code) == "GR01"] <- "G01"
levels(plants$Code)[levels(plants$Code) == "GR02"] <- "G02"

new_plants <- remove_duplicates(plants, columns = "Code")

<<<<<<< HEAD
## Just taking plants that have records in 2014 (or 2013)
year <- c(2014, 2013)

new_plants_2014 <- list()
for(list in 1:length(new_plants)){
	new_plants_2014[[list]] <- new_plants[[list]][new_plants[[list]]$YearLastRecorded %in% year,]
	
}

newdat <- as.data.frame(do.call(rbind, lapply(new_plants_2014, function(x) nrow(x))))
names(newdat)[1] <- "SpeciesRichness"
firstline <- as.data.frame(do.call(rbind, lapply(new_plants, `[`,1,)))

newdat <- cbind(newdat, firstline)

newdat <- newdat[order(newdat$Code),]

write.csv(newdat, file = "~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Papers/PlantsDatabase/PlantsDatabaseExtract_2015_11_18.csv")