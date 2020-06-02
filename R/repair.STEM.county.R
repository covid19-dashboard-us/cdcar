#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the county level covid-19 data (or similar type of data including the count time series and epidemic data) using STEM method.
#'
repair.STEM.county <- function(dat.I = list(), dat.D = NULL, h = 7){
  dat.rep.I = dat.I
  dat.rep.D = dat.D
  if(is.null(dat.I) | is.null(dat.D)){
    stop("Both the infection and death count are required to use this repairing method.")
  }

  # Infection model repairing
  n.day = ncol(dat.I) - 3
  n.loc = nrow(dat.I)
  dat.rep.I = dat.I[, -(1:3)]
  dat.rep.D = dat.D[, -(1:3)]
  for(j in 1:n.loc){
    I.tmp = dat.rep.I[j,]
    D.tmp = dat.rep.D[j,]
    ind.I = which(is.na(I.tmp))
    ind.D = which(is.na(D.tmp))
    ind.tmp = union(ind.I, ind.D)
    for(i in ind.tmp){
      I.tmp = as.matrix(dat.rep.I[j,])
      D.tmp = as.matrix(dat.rep.D[j,])
      tmp01 = as.vector(I.tmp[1:(i - 1)])
      diff.I = as.vector(tmp01[-1] - tmp01[-(i - 1)])
      tmp02 = as.vector(D.tmp[1:(i - 1)])
      diff.D = as.vector(tmp02[-1] - tmp02[-(i - 1)])
      if(sum(diff.I > 0) < (0.5*n.day) | sum(diff.I > 0) < (0.5*n.day)){
        stop("More non-zero daily observations for the infection and death count are required to use this repairing method.")
      }
      dat.fit = data.frame(I = tmp01[-1], D = tmp02[-1], lI = log(tmp01[-(i - 1)] + 1),
        diff.I = diff.I, diff.D = diff.D)
      mfit.stem.I <- gam(diff.I ~ lI, family = ziP(), data = dat.fit)
      mfit.stem.D <- gam(diff.D ~ lI, family = ziP(), data = dat.fit)
      newdata = data.frame(lI = log(tmp01[(i - 1)] + 1))
      value.I <- round(predict(mfit.stem.I, newdata, type = "response", se.fit = FALSE))
      value.D <- round(predict(mfit.stem.D, newdata, type = "response", se.fit = FALSE))
      if(is.na(value.I) | is.infinite(value.I)){value.I = 0}
      dat.rep.I[j, i] = dat.rep.I[j, (i - 1)] + value.I
      if(i < n.day){
        ind.next <- min(which(!is.na(dat.rep.I[j, -(1:i)]))) + i
        if((dat.rep.I[j, (i - 1)] + value.I > dat.rep.I[j, ind.next])){
          dat.rep.I[j, i] = round((dat.rep.I[j, ind.next] + dat.rep.I[j, (i - 1)])/2)
        }
      }
      if(is.na(value.D) | is.infinite(value.D)){value.D = 0}
      dat.rep.D[j, i] = dat.rep.D[j, (i - 1)] + value.D
      if(i < n.day){
        ind.next <- min(which(!is.na(dat.rep.D[j, -(1:i)]))) + i
        if((dat.rep.D[j, (i - 1)] + value.D > dat.rep.D[j, ind.next])){
          dat.rep.D[j, i] = round((dat.rep.D[j, ind.next] + dat.rep.D[j, (i - 1)])/2)
        }
      }
    }
    dat.rep.I = cbind(dat.I[, 1:3], dat.rep.I)
    names(dat.rep.I) = names(dat.I)
    dat.rep.D = cbind(dat.D[, 1:3], dat.rep.D)
    names(dat.rep.D) = names(dat.D)
  }
  list(dat.rep.I = dat.rep.I, dat.rep.D = dat.rep.D)
}
