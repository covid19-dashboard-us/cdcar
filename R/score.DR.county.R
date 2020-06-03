#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function detects the delay reporting issue of county/state level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
#' @import dplyr
#' @import seastests
#'
#' @param dat A data frame or list containing the time series of infection or death cases.
#' \cr
#' @param level Level of data for detection. Level "county" represents county level data, and level "state" means state level data. Default is set to "county".
#' \cr
#' @param state.show Within the dataset, which state the detection algorithm should focus on. Default is set to \code{NULL}.
#' \cr
#' @param county.show For county level data, which county the detection algorithm should focus on. Default is set to \code{NULL}.
#' \cr
#' @param log Whether perform logarithm transform for the original data. Default is set to \code{FALSE}.
#' \cr
#' @param test The test algorithm used for delay reporting detection. The available tests include the QS test (test = "qs"), Friedman test (test = "fried"), Kruskall-Wallis (test = "kw"), F-test on seasonal dummies (test = "seasdum") or the Welch test (test = "welch"). Default is set to be "wo".
#' \cr
#' @return A list containing the following information.
#' \cr
#' \code{dat.sub} The data which contains the information on the state/county specified.
#' \cr
#' \code{score} A dataframe with the second column (r.DR) indicating the percentage of counties that are detected to have delay reporting issue for each state, and the third column (score) indicating the score based on the percentage with highest "A" and lowest "C".
#' \cr
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
score.DR.county <- function(dat = list(), state.show = NULL, county.show = NULL, log.test = FALSE,  test = "wo"){
	 if(length(state.show) > 0){
		dat.sub <- dat %>% filter(State %in% state.show);
	 }else{
		dat.sub <- dat
	 }
   states = dat.sub$State
   states0 = unique(states)
   res = detect.DR.county(dat, state.show, county.show, log.test, plot.show = FALSE, test)
 	 score = data.frame(State = states0, r.DR = 0, Score = 0)
 	 for(s in states0){
  		temp = res$dr.res %>% filter(State ==s)
 	 	if(nrow(temp) == 0 ){
	  		score.s = 1
 	 	}else{
 	  		score.s = sum(temp%>% dplyr::select(DR)) / nrow(temp)
 		}
 	 	score[which(states0 ==s) , 2] = score.s
 	 }
	 score$Score = "A"
	 score$Score[score$r.DR  > 0.02] = "C"
	 score$Score[score$r.DR  <= 0.02 & score$r.DR  > 0.01] = "B"
  	 list(dat.sub = dat.sub, score = score)
}
