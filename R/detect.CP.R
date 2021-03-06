#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function detects the order-dependency violation of county/state level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
#' @import dplyr
#' @import segmented
#'
#' @param dat A data frame or list containing the time series of infection or death cases.
#' \cr
#' @param level Level of data for detection. Level "county" represents county level data, and level "state" means state level data. Default is set to "county".
#' \cr
#' @param state.show Within the dataset, which state the detection algorithm should focus on.
#' \cr
#' @param county.show For county level data, which county the detection algorithm should focus on.
#' \cr
#' @param plot.show Whether show the time series plot during the detection procedure. Default is set to \code{NULL}.
#' \cr
#' @param cp.sd Default is set to 2.
#' \cr
#' @param slope.r Default is set to 5.
#' \cr
#' @param manual.decision Whether using the interactive decision mode. Default is set to \code{FALSE}.
#' \cr
#' @return A list containing the following information.
#' \cr
#' \code{dat.sub} The data which contains the information on the state/county specified.
#' \cr
#' \code{dat.new} The data after the order-dependency violation detection. \code{NA} represents the data point that needs repairment.
#' \cr
#' \code{n.OD} Number of order-dependency violation.
#' \cr
#' \code{dates.CP} The dates in the time series that the change-point occurred.
#' \cr
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
detect.CP <- function(dat = list(), level = "county", state.show = NULL, county.show = NULL, plot.show = TRUE, cp.sd = 2, slope.r = 5, manual.decision = FALSE){
  if(level == "county"){
    res.CP = detect.CP.county(dat, state.show, plot.show, cp.sd, slope.r, manual.decision)
  }else{
    res.CP = detect.CP.state(dat, state.show, plot.show, cp.sd, slope.r, manual.decision)
  }
  return(res.CP)
}
