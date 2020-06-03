#' COVID-19 Data Checking and Reparing (CDCAR)
#'
#' This function plot the time series data of covid-19 data.
#'
#' @import plotly
#'
#' @param dat A data frame or list containing the time series to plot.
#' \cr
#' @param level Level of data repairment. Level "county" represents county level data repairment, and level "state" means state level data repairment. Default is set to "county".
#' \cr
#' @details This R package is the implementation program for article entitled "Comparing and Integrating US COVID-19 Data from Multiple Sources: A County-Level Dataset with Local Features" by Guannan Wang, Zhiling Gu, Xinyi Li, Shan Yu, Myungjin Kim, Yueying Wang, Lei Gao, and Li Wang.
#'
#' @examples
#'
#' @export
#'
plot.cdcar <- function(dat = list(), level = "state"){
  cols <- c("#045a8d", "#cc4c02")
  if(level == "state"){
    dates0 = names(dat)[-1]
    tmp01 = as.matrix(dat[, -1])
    states = dat$State
  }
  if(level == "county"){
    dates0 = names(dat)[-(1:3)]
    tmp01 = as.matrix(dat[, -(1:3)])
    states = cbind(dat$County, dat$State)
  }
  dates = as.Date(dates0, "X%Y.%m.%d")
  days = weekdays(dates)

  n = nrow(dat)
  for(i in 1:n){
    if(level == "state"){
      state.show = states[i]
    }
    if(level == "county"){
      state.show = paste0(states[i, 1], ",", states[i, 2])
    }
    counts = as.vector(tmp01[i,])
    dat.plot = data.frame(dates = dates, days = days, counts = counts)
    dat.sub = dat.plot[days %in% c("Saturday", "Sunday"), ]
    ts <- plot_ly() %>%
      add_trace(data = dat.plot,
                x = ~dates, y = ~counts, mode = "lines+markers",
                showlegend = TRUE, visible = T, name = "Week Day",
                line = list(color = cols[1], width = 1.2),
                marker = list(color = cols[1], size = 3, symbol = 'x')) %>%
      add_trace(data = dat.sub,
                x = ~dates, y = ~counts, mode = "markers",
                showlegend = TRUE, visible = T, name = "Weekend",
                marker = list(color = cols[2], size = 3, symbol = 'o'))

      xaxis <- list(title = "", showgrid = TRUE, zeroline = FALSE,
                  showline = FALSE, showticklabels = TRUE)

      yaxis <- list(title = "", showgrid = TRUE, zeroline = FALSE,
                  showline = FALSE, showticklabels = TRUE)

      ts <- ts %>%
      layout(title = paste("Time Seires:", state.show),
             xaxis = xaxis, yaxis = yaxis,
             legend = list(orientation = "h", x = 0, y = -0.1))
      print(ts)
      # orca(ts, file = paste0("plot/ts_", state.show, '.png'))
  }


}
