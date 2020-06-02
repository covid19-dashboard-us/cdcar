#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function detects the abnormal data point and/or data period of state level covid-19 data (or similar type of data including the count time series and epidemic data).
#'
detect.CP.state <- function(dat = list(), state.show = NULL, plot.show = TRUE, cp.sd = 2, slope.r = 5, manual.decision = FALSE){
  # Extract subset of the data based on state.show and county.show
  if(length(state.show) > 0){
    dat.sub <- dat %>% filter(State %in% state.show) ; # dim(dat.sub)
  }else{
    dat.sub <- dat
  }
  states = dat.sub$State
  dates0 = names(dat.sub)[-1]
  dates = as.Date(dates0, "X%Y.%m.%d")

  dat.new = dat.sub
  # CP detection
  n.day = length(dates)
  n.loc = nrow(dat.sub)
  dates.CP = matrix(0, nrow = n.loc, ncol = n.day)

  for(i in 1:nrow(dat.sub)){
    y = dat.sub [i, -c(1)]
    if(sum(!is.na(y)) == 0| sum(y >0) < 4 | length(unique(unlist(y)) )==2){
      dat.new[i, -c(1)] = y
    }else{
      ind = (min(which(y[1,]>0))):ncol(y)
      y.sub = y[1, ind]
      y.sub = as.vector(unlist(y.sub))
      x = 1:length(y.sub)
      df = data.frame("x" = x , "y" = y.sub ); colnames(df) <- c("x", "y")
      fit_lm = lm(y ~ 1 + x, data = df)
      fit_segmented= try(segmented(fit_lm, seg.Z = ~x))
      est =(fit_segmented)$psi
      slope = slope(fit_segmented)$x[,1]
      slope.inc = slope[-1] / slope[-length(slope)]
      if(!is.null(est) & max(slope.inc)> slope.r & min(est[,3]) <cp.sd) {
        cp.est = est[which(slope.inc> slope.r & est[,3]<cp.sd) ,2]
        cp.x.pos = sapply(cp.est, ceiling)
        cp = ind[cp.x.pos]
        print(paste0("An abnormal point detected at " , states[i], " on ", dates[cp] ))
        if(plot.show == TRUE){
          plot(fit_segmented$fitted.values, type = "l", main = paste(states[i]), ylab = "Cumulative Counts", xlab = "Date", axes = FALSE)
          points(df)
          lines.segmented(fit_segmented)
          points.segmented(fit_segmented)
          axis(1, x, dates[ind])
          axis(2)
          legend("topleft", paste("Slope: ", as.vector(unlist(slope)) ))
          box()
        }

        fitted = fit_segmented$fitted.values
        res = fit_segmented$residuals
        check = which(res[cp.x.pos[1] : min(cp.x.pos[1]+ 10, length(res))] * res[cp.x.pos[1]] <0)
        if(length(check) == 0){
          n.repair = 0
        }else{
          n.repair = min(check) - 1
        }

        if(manual.decision == TRUE){
          decide <- readline(prompt="Do you want to adjust this change point? [y/n]:  ")
          if(decide =="y"){
            dat.new  [i, cp:(cp+n.repair)] = fitted[cp.x.pos[1]:(cp.x.pos[1]+ n.repair)]
            dates.CP [i, cp:(cp+n.repair)]  = 1
  
            # Plot the repaired points
            plot(fit_segmented$fitted.values, type = "l", main = paste(states[i], "CP repaired"), ylab = "Cumulative Counts", xlab = "Date", axes = FALSE)
            points(df)
            points(cp.x.pos[1]:(cp.x.pos[1]+ n.repair), fitted[cp.x.pos[1]:(cp.x.pos[1]+ n.repair)], col = "red")
            lines.segmented(fit_segmented)
            points.segmented(fit_segmented)
            axis(1, x, dates[ind])
            axis(2)
            box()
          }
        }else{
          dat.new  [i, cp:(cp+n.repair)] = NA
          dates.CP [i, cp:(cp+n.repair)]  = 1
        }
      }
    }
  }
  list(dat.sub = dat.sub, dat.new = dat.new,
       dates.CP = dates.CP)
}
