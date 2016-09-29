prepareGS <- function(dat){
	dat$Study.ID <- as.factor(dat$Study.ID)
	dat$Site.ID <- as.factor(dat$Site.ID)
	dat$Location <- as.factor(dat$Location)
	dat$Latitude <- as.factor(dat$Latitude)
	dat$Longitude <- as.factor(dat$Longitude)
	dat$Habitat <- as.factor(dat$Habitat)
	dat$Taxa <- as.factor(dat$Taxa)
	dat$Taxonimic.Level <- as.factor(dat$Taxonimic.Level)
	dat$Sampled.Area.Units <- as.factor(dat$Sampled.Area.Units)
	dat$Habitat.Area.Units <- as.factor(dat$Habitat.Area.Units)
	dat$NOTES <- as.factor(dat$NOTES)
	dat$Habitat.Area <- as.numeric(as.character(dat$Habitat.Area))
	
	dat$lat <- converttoDD(dat$Latitude)
	dat$long <- converttoDD(dat$Longitude)
	
	return(dat)
}

converttoDD <- function(dms){
	index <- grep("d", dms)
	toconvert <- dms[grep("d", dms)]
	
	degrees <- regexpr("d", toconvert)
	minutes <- regexpr("m", toconvert)
	seconds <- regexpr("s", toconvert)
	
	d <- substring(toconvert, 1, degrees-1)
	m <- substring(toconvert, degrees+1, minutes-1)
	s <- substring(toconvert, minutes+1, seconds-1)
	west <- grep("W", toconvert)
	south <- grep("S", toconvert)
	
	
	d <- as.numeric(d)
	m <- as.numeric(m)
	s <- as.numeric(s)
	dd <- d + (m/60) + (s/3600)
	dd[west] <- 0-dd
	dd[south] <- 0-dd
	res <-  as.numeric(as.character(dms))
	res[index] <- dd		
	return(res)
}


OpenGS <- function(){
	require(googlesheets)
	gs_ls() ## Authentication

	garden <- gs_title("Wildlife Garden - data collection") 
	garden <- gs_read(garden, ws = "Sheet1") 
	garden <-as.data.frame(garden) 

		
	garden <- prepareGS(garden)

	write.csv(garden, file = paste("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Data/Data_extract_", Sys.Date(), ".csv", sep=""), row.names = FALSE)
	return(garden)
}
