#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the county level covid-19 data (or similar type of data including the count time series and epidemic data) using autoregressive model of count time series.
#'
repair.AR.county <- function(dat.I = list(), dat.D = NULL, h = 7){
  dat.rep.I = dat.I
  dat.rep.D = dat.D
  # Infection model repairing
  if(!is.null(dat.I)){
    n.day = ncol(dat.I) - 3
    n.loc = nrow(dat.I)
    dat.rep.I = dat.I[, -(1:3)]
    for(j in 1:n.loc){
      dat.tmp = dat.rep.I[j,]
      ind.I = which(is.na(dat.tmp))
      for(i in ind.I){
        dat.tmp = dat.rep.I[j,]
        tmp01 = as.matrix(dat.tmp[1:(i - 1)])
        diff = as.vector(tmp01 - c(0, tmp01[-(i - 1)]))
        mfit.nb = tsglm(diff, model = list(past_obs = h), distr = "nbinom")
        value.nb = predict(mfit.nb, n.ahead = 1)$median
        mfit.pois = tsglm(diff, model = list(past_obs = h), distr = "poisson")
        value.pois = predict(mfit.pois, n.ahead = 1)$median
        value = min(value.nb, value.pois, na.rm = TRUE)
        if(is.na(value) | is.infinite(value)){value = 0}
        dat.rep.I[j, i] = dat.rep.I[j, (i - 1)] + value
        if(i < n.day){
          ind.next <- min(which(!is.na(dat.rep.I[j, -(1:i)]))) + i
          if((dat.rep.I[j, (i - 1)] + value > dat.rep.I[j, ind.next])){
            dat.rep.I[j, i] = round((dat.rep.I[j, ind.next] + dat.rep.I[j, (i - 1)])/2)
          }
        }
      }
    }
    dat.rep.I = cbind(dat.I[, 1:3], dat.rep.I)
    names(dat.rep.I) = names(dat.I)
  }

  # Death data repairing
  if(!is.null(dat.D)){
    n.day = ncol(dat.D) - 3
    n.loc = nrow(dat.D)
    dat.rep.D = dat.D[, -(1:3)]
    for(j in 1:n.loc){
      dat.tmp = dat.rep.D[j,]
      ind.D = which(is.na(dat.tmp))
      for(i in ind.D){
        dat.tmp = dat.rep.D[j,]
        tmp01 = as.matrix(dat.tmp[1:(i - 1)])
        diff = as.vector(tmp01 - c(0, tmp01[-(i - 1)]))
        mfit.nb = tsglm(diff, model = list(past_obs = h), distr = "nbinom")
        value.nb = predict(mfit.nb, n.ahead = 1)$median
        mfit.pois = tsglm(diff, model = list(past_obs = h), distr = "poisson")
        value.pois = predict(mfit.pois, n.ahead = 1)$median
        value = min(value.nb, value.pois, na.rm = TRUE)
        if(is.na(value) | is.infinite(value)){value = 0}
        dat.rep.D[j, i] = dat.rep.D[j, (i - 1)] + value
        if(i < n.day){
          ind.next <- min(which(!is.na(dat.rep.D[j, -(1:i)]))) + i
          if((dat.rep.D[j, (i - 1)] + value > dat.rep.D[j, ind.next])){
            dat.rep.D[j, i] = round((dat.rep.D[j, ind.next] + dat.rep.D[j, (i - 1)])/2)
          }
        }
      }
    }
    dat.rep.D = cbind(dat.D[, 1:3], dat.rep.D)
    names(dat.rep.D) = names(dat.D)
  }
  list(dat.rep.I = dat.rep.I, dat.rep.D = dat.rep.D)
}
