#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function detects the order-dependency violation of county level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
#' @import dplyr
#'
#' @param dat A data frame or list containing the time series of infection or death cases.
#' \cr
#' @param level Level of data for detection. Level "county" represents county level data, and level "state" means state level data. Default is set to "county".
#' \cr
#' @param state.show Within the dataset, which state the detection algorithm should focus on.
#' \cr
#' @param county.show For county level data, which county the detection algorithm should focus on.
#' \cr
#' @return A list containing the following information.
#' \cr
#' \code{dat.sub} The data which contains the information on the state/county specified.
#' \cr
#' \code{dat.new} The data after the order-dependency violation detection. \code{NA} represents the data point that needs repairment.
#' \cr
#' \code{n.OD} Number of order-dependency violation.
#' \cr
#' \code{dates.OD} The dates in the time series that the order-dependency violation.
#' \cr
#' \code{diff} The daily new increasing infection/death cases for COVID-19 in US.
#' \cr
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
detect.OD <- function(dat = list(), level = "county", state.show = NULL, county.show = NULL, plot.show = NULL){
  if(level == "county"){
    res.OD = detect.OD.county(dat, state.show, county.show, plot.show)
  }else{
    res.OD = detect.OD.state(dat, state.show, plot.show)
  }
  return(res.OD)
}
