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
detect.JP.county <- function(dat = list(), window = 14){
  cutoff = 3
  d = dat[, -(1:cutoff)]
  n.day = ncol(d)
  n = nrow(d)
  dates = as.Date(names(dat[, -(1:cutoff)]), "X%Y.%m.%d")
  dailynew = cbind(0, d[, -1] - d[, -n.day])

  window1 = ceiling((window - 1)/2)
  window2 = window - window1 - 1
  dailynew.w = dailynew
  for(j in 1:n.day){
    if(j <= window1){
      start = 1
      end = start + window - 1
    }else if(j > (n.day - window2)){
      start = n.day - window + 1
      end = n.day
    }else{
      start = j - window1
      end = j + window2
    }
    # if(j <= window){
    #   start = 1
    #   end = start + window - 1
    # }else{
    #   start = j - window
    #   end = j - 1
    # }
    indj = setdiff(start:end, j)
    dailynew.w[, j] = apply(dailynew[, indj], 1, mean)
  }

  trun.scale = max(quantile(as.matrix(dailynew), probs = 0.965) + 1.5 *
    (quantile(as.matrix(dailynew), probs = 0.965) -
     quantile(as.matrix(dailynew), probs = 0.035)), 50)
  # tmp01 = as.matrix((dailynew - dailynew.w)^2/dailynew.w)
  # tmp02 = as.matrix((dailynew - dailynew.w)^2)
  tmp01 = as.matrix(dailynew/dailynew.w)
  tmp02 = as.matrix(dailynew)
  tmp01[is.infinite(tmp01)] = tmp02[is.infinite(tmp01)]
  tmp01[is.na(tmp01)] = 0
  thrd = mean(tmp01) + 3 * sd(tmp01)
  jumps = (tmp01 > thrd & dailynew > trun.scale)
  ind = which(jumps, arr.ind = TRUE)
  ind.all = ind

  for(i in 1:n){
    indi = matrix(ind[ind[, 1] == i,], ncol = 2)
    if(nrow(indi) > 3){
      ind.all = ind[ind[, 1] != i,]
    }
    if(nrow(indi) > 0 & nrow(indi) < 10){
      plot(dates, as.numeric(dailynew[i,]), type = "l", main = paste(dat[i, 2], ",", dat[i, 3]), ylab = "Daily Increases")
      for(j in 1:nrow(indi)){
        points(dates[indi[j, 2]], dailynew[i, indi[j, 2]], col = 2)
        text(dates[indi[j, 2]], y = dailynew[i, indi[j, 2]], labels = dates[indi[j, 2]])
      }
    }
  }

  for(i in 1:nrow(ind.all)){
    indi = ind.all[i,]
    print(paste(dat[indi[1], 2], ",", dat[indi[1], 3], "jump on ", dates[indi[2]], "dailynew being", dailynew[indi[1], indi[2]]))
  }
  n.JP = nrow(ind.all)
  cat("# of jumps =", n.JP, "\n")

  dat.new = d
  dat.new[ind.all] = NA
  names(dat.new) = dates
  dat.new = cbind(dat[, (1:cutoff)], dat.new)
  names(jumps) = dates
  jumps = cbind(dat[, (1:cutoff)], jumps)
  return(list(dat.new = dat.new, jumps = jumps, n.JP = n.JP))
}
