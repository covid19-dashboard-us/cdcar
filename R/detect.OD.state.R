#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function detects the order-dependency violation of state level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
detect.OD.state <- function(dat = list(), state.show = NULL, plot.show = NULL){
  # Extract subset of the data based on state.show and county.show
  if(length(state.show) > 0){
    dat.sub <- dat %>% filter(State %in% state.show)
  }else{
    dat.sub <- dat
  }
  states = dat.sub$State
  dates0 = names(dat.sub)[-1]
  dates = as.Date(dates0, "X%Y.%m.%d")

  # OD Voilation
  n.day = length(dates)
  n.loc = nrow(dat.sub)
  dates.OD = matrix(0, nrow = n.loc, ncol = n.day)
  for(i in 2:n.day){
    ind.tmp = 1:(i - 1)
    dates.OD.tmp = dates.OD[, ind.tmp]
    dat.curr = matrix(rep(dat.sub[, (i + 1)], (i - 1)), ncol = (i - 1))
    dat.past = as.matrix(dat.sub[, (ind.tmp + 1)])
    diff = dat.curr - dat.past
    dates.OD.tmp[diff < 0] = 1
    dates.OD[, ind.tmp] = dates.OD.tmp
  }
  n.OD = apply(dates.OD, 1, sum)
  dat.new = dat.sub[, -1]
  dat.new[dates.OD == 1] = NA
  dates.OD = cbind(dat.sub[, 1], dates.OD)
  names(dates.OD) = names(dat.sub)
  dat.new = cbind(dat.sub[, 1], dat.new)
  names(dat.new) = names(dat.sub)

  diff = dat.sub[, -1] - cbind(rep(0, n.loc), dat.sub[, -c(1, 1 + n.day)])
  diff = cbind(dat.sub[, 1], diff)
  names(diff) = names(dat.sub)

  # Plots
  if(!is.null(plot.show)){
    if(plot.show == "all"){
      for (i in 1:nrow(dat.sub)){
        tmp01 = dat.sub[i, -1]
        tmp02 = diff[i, -1]
        lb = min(tmp01, tmp02) * 1.2
        ub = max(tmp01, tmp02) * 1.2
        plot(x = dates, y = tmp01, type = "l", ylim = c(lb, ub), xlab = "", ylab = "", col = rgb(.8,.8,.8), main = paste0(states[i], ", ", "Daily New & Cumulative"))
        points(dates[tmp02 < 0], tmp01[tmp02 < 0], lwd = 3, col = 4)
        # axis(side = 4, at = pretty(c(lb, ub)), col = rgb(.8,.8,.8), col.axis = rgb(.8,.8,.8))
        # par(new = TRUE)
        lines(x = dates, y = tmp02, type = "l", xlab = "", ylab = "", ylim = c(lb, ub))
        points(dates[tmp02 < 0], tmp02[tmp02 < 0], lwd = 3, col = 6)
        abline(h = 0, lty = 3, col = 2, lwd = 2)
      }
    }else{
      dat.plot = dat.sub[n.OD > 0,]
      diff.plot = diff[n.OD > 0,]
      for(i in 1:nrow(dat.plot)){
        tmp01 = dat.plot[i, -(1)]
        tmp02 = diff.plot[i, -(1)]
        lb = min(tmp01, tmp02) * 1.2
        ub = max(tmp01, tmp02) * 1.2
        plot(x = dates, y = tmp01, type = "l", ylim = c(lb, ub), xlab = "", ylab = "", col = rgb(.8,.8,.8), main = paste0(states[i],  ", ", "Daily New & Cumulative"))
        points(dates[tmp02 < 0], tmp01[tmp02 < 0], lwd = 3, col = 4)
        # axis(side = 4, at = pretty(c(lb, ub)), col = rgb(.8,.8,.8), col.axis = rgb(.8,.8,.8))
        # par(new = TRUE)
        lines(x = dates, y = tmp02, type = "l", xlab = "", ylab = "", ylim = c(lb, ub))
        points(dates[tmp02 < 0], tmp02[tmp02 <0], lwd = 3, col = 6)
        abline(h = 0, lty = 3, col = 2, lwd = 2)
      }
    }}

  list(dat.sub = dat.sub, dat.new = dat.new,
       n.OD = n.OD, dates.OD = dates.OD, diff = diff)
}
