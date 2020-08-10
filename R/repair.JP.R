#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the abnormal data in covid-19 data (or similar type of data including the count time series and epidemic data).
#'
#' @import tscount
#' @import mgcv
#' @importFrom Triangulation TriMesh TriPlot
#' @importFrom BPST basis inVT
#'
#' @param dat.I.JP A data frame or list containing the time series of infected cases with outliers. \code{NA} in the dataset indicates the data points that need repairment.
#' \cr
#' @param dat.I.JP A data frame or list containing the time series of death cases with outliers. \code{NA} in the dataset indicates the data points that need repairment.
#' \cr
#' @param dat.I A data frame or list containing the time series of infected cases.
#' \cr
#' @param dat.D A data frame or list containing the time series of death cases.
#' \cr
#' @param h Parameter used in data repairment. For repair methods "AR" and "ARMA", this parameter controls the number of lags in time series modeling. For repair methods "CLEP" and "STEM", this parameter controls the time window for model fitting.
#' \cr
#' @param level Level of data repairment. Level "county" represents county level data repairment, and level "state" means state level data repairment. Default is set to "county".
#' \cr
#' @param date.start Trace back date. Default is "2020-03-01".
#'
#' @return A data frame or list containing the repaired time series, including the infection cases and/or death cases.
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
repair.JP <- function(dat.I.JP = list(), dat.D.JP = list(), dat.I = list(), dat.D = list(), level = "county", h = 7, date.start = as.Date("2020-03-01")){
  if(level == "state"){
    dat.rep = repair.JP.state(dat.I.JP, dat.D.JP, dat.I, dat.D, h, date.start)
  }
  if(level == "county"){
    dat.rep = repair.JP.county(dat.I.JP, dat.D.JP, dat.I, dat.D, h, date.start)
  }
  return(dat.rep)
}
