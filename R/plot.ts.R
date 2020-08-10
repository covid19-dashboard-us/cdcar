library(plotly)
plot.ts <- function(loc.show, dat = list(), title = "", filename = "plot/ts_"){
  cols <- c("#045a8d", "#cc4c02")
  dat = dat[dat$date > as.Date("2020-03-15"), ]
  dat.sub = dat[dat$day %in% c("Saturday", "Sunday"), ]
  dat.sun = dat[dat$day %in% "Sunday", ]
  ts <- plot_ly() %>%
    add_trace(data = dat,
      x = ~date, y = ~count, mode = "lines+markers",
      showlegend = TRUE, visible = T, name = "Week Day",
      line = list(color = cols[1], width = 1.2),
      marker = list(color = cols[1], size = 3, symbol = 'x')) %>%
    add_trace(data = dat.sub,
      x = ~date, y = ~count, mode = "markers",
      showlegend = TRUE, visible = T, name = "Weekend",
      marker = list(color = cols[2], size = 3, symbol = 'o'))

  xaxis <- list(title = "", showgrid = TRUE, zeroline = FALSE,
                showline = FALSE, showticklabels = TRUE)

  yaxis <- list(title = "", showgrid = TRUE, zeroline = FALSE,
                showline = FALSE, showticklabels = TRUE)
  ts <- ts %>%
    layout(title = paste(title, loc.show),
           xaxis = xaxis, yaxis = yaxis,
           legend = list(orientation = "h", x = 0, y = -0.1))

  n.sun = sum(dat$day == "Sunday")
  for(i in 1:n.sun){
    ts = ts %>% add_segments(x = dat.sun$date[i], xend = dat.sun$date[i], y = 0, yend = max(dat$count),
                             inherit = FALSE, showlegend = FALSE,
                             line = list(color = cols[2], dash = "dot", width = 1))
  }

  print(ts)
  orca(ts, file = paste0(filename, loc.show, ".png"))
}
