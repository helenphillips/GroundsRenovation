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


new_plants <- remove_duplicates(plants, columns = "Code")


newdat <- as.data.frame(do.call(rbind, lapply(new_plants, function(x) nrow(x))))
newdat$code <- rownames(newdat)
names(newdat)[1] <- "SpeciesRichness"
newdat <- newdat[order(newdat$code),]

firstline <- as.data.frame(do.call(rbind, lapply(new_plants, `[`,1,)))

newdat$Habitat <- firstline$Habitat[order(firstline$Code)]


write.csv(newdat, file = "~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Papers/PlantsDatabase/PlantsDatabaseExtract_2015_11_18.csv")