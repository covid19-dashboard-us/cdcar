#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function detects the delay reporting issue of county level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
detect.DR.county <- function(dat = list(), state.show = NULL, county.show = NULL, log.test = FALSE,
							plot.show = FALSE,  test = "wo"){
	# Extract subset of the data based on state.show and county.show
	if(length(state.show) > 0){
		dat.sub <- dat %>% filter(State %in% state.show) ; # dim(dat.sub)
	}else{
		dat.sub <- dat
	}
	if (length(county.show) > 0){
		dat.sub = dat.sub %>% filter (County %in% county.show)
	}
	states = dat.sub$State
	counties = dat.sub$County
	dates0 = names(dat.sub)[-c(1:3)]
	dates = as.Date(dates0, "X%Y.%m.%d")

	dr.res = dat.sub %>% mutate(DR= 0) %>% dplyr::select("State","County", "DR")

	for(i in 1:nrow( dat.sub )){
		print(paste0(counties[i],",", states[i]))
		x = as.vector(unlist(dat.sub[i, -c(1:3)]))

		if(sum(x>0) > 14 & sum(!is.na(x)) > 0 ){
			myts = ts (x,frequency =7)
			logts = ts (log(x[which(x>0)]),frequency =7)
			if(log.test == TRUE){
				d = logts
			}else{
				d = myts
			}
			dr.res[i,3]= try(isSeasonal(d, test = test, freq = 7))

			if(dr.res[i,3] == TRUE & plot.show == TRUE ){
				ts = d %>%
					  stl(s.window = "periodic") %>%
					  autoplot()
				print(ts)
			}
		}else{
			warning(paste0(counties[i], "," ,states[i], "'s positive sequence shorter than two weeks."))
			dr.res[i,3]= 0
		}
	}
	list(dat.sub = dat.sub, dr.res = dr.res)
}
