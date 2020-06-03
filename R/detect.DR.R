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
#' @param plot.show Whether output plots to the console for sequence that is tested to have delay-reporting issue. Default is set to \code{FALSE}.
#' \cr
#' @param test The test algorithm used for delay reporting detection. The available tests include the QS test (test = "qs"), Friedman test (test = "fried"), Kruskall-Wallis (test = "kw"), F-test on seasonal dummies (test = "seasdum") or the Welch test (test = "welch"). Default is set to be "wo".
#' \cr
#' @return A list containing the following information.
#' \cr
#' \code{dat.sub} The data which contains the information on the state/county specified.
#' \cr
#' \code{dr.res} A dataframe with the last column indicating whether the state/county has delay-reporting issue (0=No, 1=Yes).
#' \cr
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
detect.DR <- function(dat = list(), level = "county", state.show = NULL, county.show = NULL, log = FALSE, plot.show = FALSE,  test = "wo"){
  if(level == "county"){
    res.DR = detect.DR.county(dat, state.show, county.show, log,  plot.show,  test)
  }else{
    res.DR = detect.DR.state(dat, state.show, log, plot.show, test)
  }
  return(res.DR)
}
