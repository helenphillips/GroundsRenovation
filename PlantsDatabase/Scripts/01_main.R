##############
## FUNCTIONS
##############


source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/RemoveDuplicates.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/SpeciesSimilarity.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Functions/SpeciesOverTime.R")

##############
## libraries
##############

library(lattice)
library(ggplot2)

##############
## FOLDERS
##############

figure_out <- "~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Scripts/Figures"



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

## Check how many are duplicates in each area
l <- aggregate(plants$NBN.Name, list(plants$Code), function(x){length = length(x)})
u <- aggregate(plants$NBN.Name, list(plants$Code), function(x){unique = length(unique(x))})
areas <- cbind(l, u$x)


new_plants <- remove_duplicates(plants, columns = "Habitat")


## Check that length of each habitat is expected
#t <- as.data.frame(do.call(rbind, lapply(new_plants, function(x) nrow(x))))
#t$Group <- rownames(t)
#t <- t[order(t$Group),]
#areas <- cbind(areas, t$V1)
## Which it is




############################
## Number of Similar Species
############################

similarity_habitat <- species_similarity(new_plants)

png(file.path(figure_out, "HabitatSimiarity.png"), height = 700, width = 700)
levelplot(similarity_habitat, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45, cex=1.1), y=list(cex=1.1)), ylab = "", xlab = "",cex.txt=1.5)
dev.off()


new_plants_area <- remove_duplicates(plants, columns = "Code")
similarity_area <- species_similarity(new_plants_area)
png(file.path(figure_out, "AreaSimiarity.png"), height = 1000, width = 1000)
levelplot(similarity_area, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45)), ylab = "", xlab = "")
dev.off()



############################
## Species over time
############################


y.axis_area <- seq(1, length(new_plants_area), 1)
x.axis <- c(1995, 2015)

Species_over_time(new_plants_area, x.axis, y.axis_areas)
y.axis <- seq(1, length(new_plants), 1)
Species_over_time(new_plants, x.axis, y.axis)






