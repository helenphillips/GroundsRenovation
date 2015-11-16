

##############
## FUNCTIONS
##############


source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/RemoveDuplicates.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/SpeciesSimilarity.R")



library(lattice)
##############
## DATA
##############


plants <- read.csv("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/DataFiles/PlantsByBed_NBN_Combined_2015-11-05-CMTR.csv")
plants$X <- NULL
nrow(plants) # 5731

plants <- plants[plants$NBN.TVK != "",]
nrow(plants) #5459 

names(plants)[1:2] <- c("ID", "Habitat")


## Check how many are duplicates in each area
l <- aggregate(plants$NBN.Name, list(plants$Code), function(x){length = length(x)})
u <- aggregate(plants$NBN.Name, list(plants$Code), function(x){unique = length(unique(x))})
areas <- cbind(l, u$x)


new_plants <- remove_duplicates(plants)


## Check that length of each habitat is expected
t <- as.data.frame(do.call(rbind, lapply(new_plants, function(x) nrow(x))))
t$Group <- rownames(t)
t <- t[order(t$Group),]
areas <- cbind(areas, t$V1)
## Which it is




############################
## Number of Similar Species
############################

similarity <- species_similarity(new_plants)
levelplot(similarity)