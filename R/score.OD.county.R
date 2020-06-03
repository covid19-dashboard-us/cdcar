#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function provides the score of a dataset based on the order-dependency violation of county level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
#' @param dat A data frame or list containing the time series of infection or death cases.
#' \cr
#' @param state.show Within the dataset, which state the detection algorithm should focus on.
#' \cr
#' @param county.show For county level data, which county the detection algorithm should focus on.
#' \cr
#' @return A list containing the following information.
#' \cr
#' \code{dat.sub} The data which contains the information on the state/county specified.
#' \cr
#' \code{score} The score for the dataset.
#' \cr
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
score.OD.county <- function(dat = list(), state.show = NULL, county.show = NULL){
	# Extract subset of the data based on state.show and county.show
	if(length(state.show) > 0){
		dat.sub <- dat %>% filter(State %in% state.show)
	}else{
		dat.sub <- dat
	}
  	states = dat$State
  	res = detect.OD.county(dat, states, county.show)
  	score = res$dat.sub[, 1:3]
  	n.day = ncol(res$dat.sub) - 3
  	score$r.OD = res$n.OD/n.day
  	score$Score = "A"
  	score$Score[score$r.OD > 0.0005] = "C"
  	score$Score[score$r.OD <= 0.0005 & score$r.OD > 0.0001] = "B"
  	list(dat.sub = dat.sub, score = score)
}
