#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the abnormal data in covid-19 data (or similar type of data including the count time series and epidemic data).
#'
#' @import tscount
#' @import mgcv
#' @importFrom Triangulation TriMesh TriPlot
#' @importFrom BPST basis inVT
#'
#' @param dat.I A data frame or list containing the time series of infection cases. \code{NA} in the dataset indicates the data points that need repairment.
#' \cr
#' @param dat.D A data frame or list containing the time series of death cases. Default sets to \code{NULL}. Repair method "STEM" requires both infection cases and death cases for the repairment.
#' \cr
#' @param h Parameter used in data repairment. For repair methods "AR" and "ARMA", this parameter controls the number of lags in time series modeling. For repair methods "CLEP" and "STEM", this parameter controls the time window for model fitting.
#' \cr
#' @param level Level of data repairment. Level "county" represents county level data repairment, and level "state" means state level data repairment. Default is set to "county".
#' \cr
#' @param method Method used for data repairment. "AR" represents autoregressive model of count time series, "ARMA" represents the autoregressive moving average models of count time series, "CLEP" represents the combined linear and exponential method, "STEM" represents the spatio-temporal epidemic method. Default is set to "AR".
#'
#' @return A data frame or list containing the repaired time series, including the infection cases and/or death cases.
#'
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
repair.cdcar <- function(dat.I = list(), dat.D = NULL, h = 7, level = "county", method = "AR"){
  if(level == "state"){
    if(method == "ARMA"){
      dat.rep = repair.ARMA.state(dat.I, dat.D, h)
    }else if(method == "CLEP"){
      dat.rep = repair.CLEP.state(dat.I, dat.D, h)
    }else if(method == "STEM"){
      dat.rep = repair.STEM.state(dat.I, dat.D, h)
    }else{
      dat.rep = repair.AR.state(dat.I, dat.D, h)
    }
  }
  if(level == "county"){
    if(method == "ARMA"){
      dat.rep = repair.ARMA.county(dat.I, dat.D, h)
    }else if(method == "CLEP"){
      dat.rep = repair.CLEP.county(dat.I, dat.D, h)
    }else if(method == "STEM"){
      dat.rep = repair.STEM.county(dat.I, dat.D, h)
    }else{
      dat.rep = repair.AR.county(dat.I, dat.D, h)
    }
  }
  return(dat.rep)
}
