Species_over_time<- function(data_list, x.axis, y.axis){
	transparent_black <- rgb(0, 0, 0, alpha = 0.2)
	plot(1, type="n", xlab="", ylab="", ylim = range(y.axis), xlim = x.axis, yaxt = "n")
	axis(2, at=1:length(data_list), labels=names(data_list),las=2)

	for(list in 1:length(data_list)){
		y <- y.axis[list]
	
		for(species in 1:nrow(data_list[[list]])){
			years <- seq(data_list[[list]]$YearFirstRecorded[species], data_list[[list]]$YearLastRecorded[species])
			points(jitter(rep(y, length(years))) ~ jitter(years), pch = 19, col = transparent_black)
	
		}
	}
}