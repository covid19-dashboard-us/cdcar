#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function repairs the state level covid-19 data (or similar type of data including the count time series and epidemic data) using CLEP method.
#'
repair.CLEP.state <- function(dat.I = list(), dat.D = NULL, h = 5){
  dat.rep.I = dat.I
  dat.rep.D = dat.D
  # Infection model repairing
  if(!is.null(dat.I)){
    n.day = ncol(dat.I) - 1
    n.loc = nrow(dat.I)
    dat.rep.I = dat.I[, -1]
    for(j in 1:n.loc){
      dat.tmp = dat.rep.I[j,]
      ind.I = which(is.na(dat.tmp))
      for(i in ind.I){
        dat.tmp = dat.rep.I[j,]
        tmp01 = as.matrix(dat.tmp[1:(i - 1)])
        diff = as.vector(tmp01 - c(0, tmp01[-(i - 1)]))
        t = 1:h
        newdata = data.frame(t = (h + 1))
        mfit1 = glm(diff[(i - h):(i - 1)] ~ t, family = "poisson")
        value1 = round(predict(mfit1, newdata = newdata, type = "response"))
        mfit5 = lm(diff[(i - h):(i - 1)] ~ t)
        value5 = round(predict(mfit5, newdata = newdata, type = "response"))
        lI = log(diff[(i - h - 1):(i - 1 - 1)] + 1)
        newdata = data.frame(lI = log(diff[i - 1] + 1))
        mfit2 = glm(diff[(i - h):(i - 1)] ~ lI, family = "poisson")
        value2 = round(predict(mfit2, newdata = newdata, type = "response"))
        tmp02 = c(value1, value5, value2)
        tmp02[tmp02 < 0] = 0
        value = round(mean(tmp02, na.rm = TRUE))
        if(is.na(value) | is.infinite(value)){value = 0}
        dat.rep.I[j, i] = dat.rep.I[j, (i - 1)] + value
        if(dat.rep.I[j, (i - 1)] + value < dat.rep.I[j, (i - 1)]){
          dat.rep.I[j, i] = dat.rep.I[j, (i - 1)]
        }
        if(i < n.day){
          ind.next <- min(which(!is.na(dat.rep.I[j, -(1:i)]))) + i
          if((dat.rep.I[j, (i - 1)] + value > dat.rep.I[j, ind.next])){
            dat.rep.I[j, i] = round((dat.rep.I[j, ind.next] + dat.rep.I[j, (i - 1)])/2)
          }
        }
      }
    }
    dat.rep.I = cbind(dat.I[, 1], dat.rep.I)
    names(dat.rep.I) = names(dat.I)
  }

  # Death data repairing
  if(!is.null(dat.D)){
    n.day = ncol(dat.D) - 1
    n.loc = nrow(dat.D)
    dat.rep.D = dat.D[, -1]
    for(j in 1:n.loc){
      dat.tmp = dat.rep.D[j,]
      ind.D = which(is.na(dat.tmp))
      for(i in ind.D){
        dat.tmp = dat.rep.D[j,]
        tmp01 = as.matrix(dat.tmp[1:(i - 1)])
        diff = as.vector(tmp01 - c(0, tmp01[-(i - 1)]))
        t = 1:h
        newdata = data.frame(t = (h + 1))
        mfit1 = glm(diff[(i - h):(i - 1)] ~ t, family = "poisson")
        value1 = round(predict(mfit1, newdata = newdata, type = "response"))
        mfit5 = lm(diff[(i - h):(i - 1)] ~ t)
        value5 = round(predict(mfit5, newdata = newdata, type = "response"))
        lI = log(diff[(i - h - 1):(i - 1 - 1)] + 1)
        newdata = data.frame(lI = log(diff[i - 1] + 1))
        mfit2 = glm(diff[(i - h):(i - 1)] ~ lI, family = "poisson")
        value2 = round(predict(mfit2, newdata = newdata, type = "response"))
        tmp02 = c(value1, value5, value2)
        tmp02[tmp02 < 0] = 0
        value = round(mean(tmp02, na.rm = TRUE))
        if(is.na(value) | is.infinite(value)){value = 0}
        dat.rep.D[j, i] = dat.rep.D[j, (i - 1)] + value
        if(dat.rep.D[j, (i - 1)] + value < dat.rep.D[j, (i - 1)]){
          dat.rep.D[j, i] = dat.rep.D[j, (i - 1)]
        }
        if(i < n.day){
          ind.next <- min(which(!is.na(dat.rep.D[j, -(1:i)]))) + i
          if((dat.rep.D[j, (i - 1)] + value > dat.rep.D[j, ind.next])){
            dat.rep.D[j, i] = round((dat.rep.D[j, ind.next] + dat.rep.D[j, (i - 1)])/2)
          }
        }
      }
    }
    dat.rep.D = cbind(dat.D[, 1], dat.rep.D)
    names(dat.rep.D) = names(dat.D)
  }
  list(dat.rep.I = dat.rep.I, dat.rep.D = dat.rep.D)
}
