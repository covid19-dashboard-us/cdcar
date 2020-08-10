#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the state level covid-19 data (or similar type of data including the count time series and epidemic data) using autoregressive model of count time series.
#'
repair.JP.state <- function(dat.I.JP = list(), dat.D.JP = list(), dat.I = list(), dat.D = list(), h = 7, date.start = as.Date("2020-03-01")){
  dat.rep.state = repair.cdcar(dat.I = dat.I.JP, dat.D = dat.D.JP, h = h, level = "state", method = "CLEP")
  dat.I.JPrep = dat.rep.state$dat.rep.I
  dat.D.JPrep = dat.rep.state$dat.rep.D
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
    Dnew.rep[indi[1], as.character(date.end + 1)] = Dnew[indi[1], as.character(date.end + 1)]
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

  list(dat.rep.I = dat.I, dat.rep.D = dat.D.final,
       dat.I = dat.I, dat.D = dat.D)
}
