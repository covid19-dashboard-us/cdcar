#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the state level covid-19 data (or similar type of data including the count time series and epidemic data) using autoregressive model of count time series.
#'
repair.JP.state <- function(dat.I.JP = list(), dat.D.JP = list(), dat.I = list(), dat.D = list(), h = 7, date.start = as.Date("2020-03-01")){
  dat.rep.state = repair.cdcar(dat.I = dat.I.JP, dat.D = dat.D.JP, h = h, level = "state", method = "ARIMA")
  dat.I.JPrep = dat.rep.state$dat.rep.I
  dat.D.JPrep = dat.rep.state$dat.rep.D

  names(dat.I) = names(dat.I.JP)
  ind.I = which(is.na(dat.I.JP), arr.ind = TRUE)
  ind.I = matrix(ind.I[order(ind.I[, 1]), ], ncol = 2)
  n.I = nrow(ind.I)

  dates = as.Date(names(dat.I.JP)[-1])
  n.day = length(dates)
  Inew = cbind(0, dat.I[, -(1:2)] - dat.I[, -c(1, (n.day + 1))])
  names(Inew) = dates
  Inew.rep = cbind(0, dat.I.JPrep[, -(1:2)] - dat.I.JPrep[, -c(1, (n.day + 1))])
  Iadd = 0 * Inew
  dat.I.final = dat.I.JPrep
  for(i in 1:n.I){
    indi = matrix(ind.I[i, ], ncol = 2)
    date.end = dates[indi[2] - 1]
    var.names = as.character(seq(date.start, date.end, by = 1))
    Inew.rep[indi[1], as.character(date.end + 1)] = Inew[indi[1], as.character(date.end + 1)]
    Inew.tmp01 = Inew[indi[1], var.names]
    Inew.tmp02 = Inew.rep[indi[1], var.names]
    ni = length(var.names)
    diffi = as.numeric(Inew.tmp01[ni] - Inew.tmp02[ni])
    Inew.tmp03 = round(Inew.tmp02/sum(Inew.tmp02) * diffi)
    if((diffi - sum(Inew.tmp03)) < 0){
      ind.nz = (1:ni)[Inew.tmp03 > 0]
      Inew.tmp03[ind.nz[1:(sum(Inew.tmp03) - diffi)]] = Inew.tmp03[ind.nz[1:(sum(Inew.tmp03) - diffi)]] - 1
    }else{
      Inew.tmp03[ni] = Inew.tmp03[ni] + diffi - sum(Inew.tmp03)
    }
    Iadd[indi[1], var.names] = cumsum(matrix(Inew.tmp03, nrow = 1))
    Inew.rep[indi[1], var.names] = Inew.rep[indi[1], var.names] + Inew.tmp03
    Inew[indi[1], var.names] = Inew.rep[indi[1], var.names]
    dat.I.final[indi[1], var.names] = cumsum(matrix(Inew.rep[indi[1], var.names], nrow = 1))

    plot(dates, as.numeric(dat.I[indi[1], -1]), type = "l", ylab = "Cum Cases", main = as.character(dat.I[indi[1], 1]))
    lines(dates, as.numeric(dat.I.final[indi[1], -1]), col = 2)
    points(dates, as.numeric(dat.I.final[indi[1], -1]), pch = "*", col = 2)
  }

  names(dat.D) = names(dat.D.JP)
  ind.D = which(is.na(dat.D.JP), arr.ind = TRUE)
  ind.D = matrix(ind.D[order(ind.D[, 1]), ], ncol = 2)
  n.D = nrow(ind.D)

  dates = as.Date(names(dat.D.JP)[-1])
  n.day = length(dates)
  Dnew = cbind(0, dat.D[, -(1:2)] - dat.D[, -c(1, (n.day + 1))])
  names(Dnew) = dates
  Dnew.rep = cbind(0, dat.D.JPrep[, -(1:2)] - dat.D.JPrep[, -c(1, (n.day + 1))])
  Dadd = 0 * Dnew
  dat.D.final = dat.D.JPrep
  for(i in 1:n.D){
    indi = matrix(ind.D[i, ], ncol = 2)
    date.end = dates[indi[2] - 1]
    var.names = as.character(seq(date.start, date.end, by = 1))
    if((indi[2] - 1) < n.day){
      Dnew.rep[indi[1], as.character(date.end + 1)] = Dnew[indi[1], as.character(date.end + 1)]
    }else{
      Dnew.rep[indi[1], as.character(date.end)] = Dnew[indi[1], as.character(date.end)]
    }
    Dnew.tmp01 = Dnew[indi[1], var.names]
    Dnew.tmp02 = Dnew.rep[indi[1], var.names]
    ni = length(var.names)
    diffi = as.numeric(Dnew.tmp01[ni] - Dnew.tmp02[ni])
    Dnew.tmp03 = round(Dnew.tmp02/sum(Dnew.tmp02) * diffi)
    if((diffi - sum(Dnew.tmp03)) < 0){
      ind.nz = (1:ni)[Dnew.tmp03 > 0]
      Dnew.tmp03[ind.nz[1:(sum(Dnew.tmp03) - diffi)]] = Dnew.tmp03[ind.nz[1:(sum(Dnew.tmp03) - diffi)]] - 1
    }else{
      Dnew.tmp03[ni] = Dnew.tmp03[ni] + diffi - sum(Dnew.tmp03)
    }
    Dadd[indi[1], var.names] = cumsum(matrix(Dnew.tmp03, nrow = 1))
    Dnew.rep[indi[1], var.names] = Dnew.rep[indi[1], var.names] + Dnew.tmp03
    Dnew[indi[1], var.names] = Dnew.rep[indi[1], var.names]
    dat.D.final[indi[1], var.names] = cumsum(matrix(Dnew.rep[indi[1], var.names], nrow = 1))

    plot(dates, as.numeric(dat.D[indi[1], -1]), type = "l", ylab = "Cum Cases", main = as.character(dat.D[indi[1], 1]))
    lines(dates, as.numeric(dat.D.final[indi[1], -1]), col = 2)
    points(dates, as.numeric(dat.D.final[indi[1], -1]), pch = "*", col = 2)
  }

  list(dat.rep.I = dat.I.final, dat.rep.D = dat.D.final,
       dat.I = dat.I, dat.D = dat.D)
}
