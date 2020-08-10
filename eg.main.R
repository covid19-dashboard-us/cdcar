rm(list = ls())

# Packages
library(cdcar)
library(dplyr)
library(mgcv)
library(plotly)
library(seastests)
library(segmented)
library(tscount)

date.update = as.Date("2020-07-26")

######################################################################
# Step 0. Read in Data
source.county = c("NYT", "JHU", "USAFacts")
source.state = c("Atlantic", "NYT", "JHU", "USAFacts")

# Compare infection state data
for (i.s in source.county[1]){
  I.county <- read.delim(paste0("data/", i.s, "_all_Infected_county_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  D.county <- read.delim(paste0("data/", i.s, "_all_Death_county_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  I.county = eval(parse(text = paste0("I.county.", i.s, " = I.county[,c(1:3, ncol(I.county):4)]")))
  D.county = eval(parse(text = paste0("D.county.", i.s, " = D.county[,c(1:3, ncol(I.county):4)]")))
}

for (i.s in source.state[2]){
  I.state <- read.delim(paste0("data/", i.s, "_all_Infected_state_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  D.state <- read.delim(paste0("data/", i.s, "_all_Death_state_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  I.state = eval(parse(text = paste0("I.state.", i.s, " = I.state[,c(1, ncol(I.state):2)]")))
  D.state = eval(parse(text = paste0("D.state.", i.s, " = D.state[,c(1, ncol(D.state):2)]")))
}

state = unique(I.state$State)
n.day = ncol(I.state) - 1
n.state = length(state)
date = substring(names(I.state[, -1]), 2)
date = as.Date(gsub('\\.', '-', date))
day = weekdays(date)
tmp = as.matrix(I.state[, -1])
Inew.state = I.state[, -1] - cbind(0, I.state[, -c(1, (n.day + 1))])
tmp.mon = Inew.state[, day == "Monday"]
tmp.tue = Inew.state[, day == "Tuesday"]
tmp.wed = Inew.state[, day == "Wednesday"]
tmp.thu = Inew.state[, day == "Thursday"]
tmp.fri = Inew.state[, day == "Friday"]
tmp.sat = Inew.state[, day == "Saturday"]
tmp.sun = Inew.state[, day == "Sunday"]
mean.state.mon = apply(tmp.mon, 1, mean)
mean.state.tue = apply(tmp.tue, 1, mean)
mean.state.wed = apply(tmp.wed, 1, mean)
mean.state.thu = apply(tmp.thu, 1, mean)
mean.state.fri = apply(tmp.fri, 1, mean)
mean.state.sat = apply(tmp.sat, 1, mean)
mean.state.sun = apply(tmp.sun, 1, mean)
mean.mon = mean(mean.state.mon)
mean.tue = mean(mean.state.tue)
mean.wed = mean(mean.state.wed)
mean.thu = mean(mean.state.thu)
mean.fri = mean(mean.state.fri)
mean.sat = mean(mean.state.sat)
mean.sun = mean(mean.state.sun)
c(mean.mon, mean.tue, mean.wed, mean.thu, mean.fri, mean.sat, mean.sun)
Imean.state = cbind(mean.state.mon, mean.state.tue, mean.state.wed,
                    mean.state.thu, mean.state.fri, mean.state.sat,
                    mean.state.sun)
Imean.state = data.frame(State = I.state[, 1], Imean.state = Imean.state)
write.csv(Imean.state, "data/Imean.state.csv", row.names = FALSE)

Dnew.state = D.state[, -1] - cbind(0, D.state[, -c(1, (n.day + 1))])
tmp.mon = Dnew.state[, day == "Monday"]
tmp.tue = Dnew.state[, day == "Tuesday"]
tmp.wed = Dnew.state[, day == "Wednesday"]
tmp.thu = Dnew.state[, day == "Thursday"]
tmp.fri = Dnew.state[, day == "Friday"]
tmp.sat = Dnew.state[, day == "Saturday"]
tmp.sun = Dnew.state[, day == "Sunday"]
mean.state.mon = apply(tmp.mon, 1, mean)
mean.state.tue = apply(tmp.tue, 1, mean)
mean.state.wed = apply(tmp.wed, 1, mean)
mean.state.thu = apply(tmp.thu, 1, mean)
mean.state.fri = apply(tmp.fri, 1, mean)
mean.state.sat = apply(tmp.sat, 1, mean)
mean.state.sun = apply(tmp.sun, 1, mean)
mean.mon = mean(mean.state.mon)
mean.tue = mean(mean.state.tue)
mean.wed = mean(mean.state.wed)
mean.thu = mean(mean.state.thu)
mean.fri = mean(mean.state.fri)
mean.sat = mean(mean.state.sat)
mean.sun = mean(mean.state.sun)
c(mean.mon, mean.tue, mean.wed, mean.thu, mean.fri, mean.sat, mean.sun)
Dmean.state = cbind(mean.state.mon, mean.state.tue, mean.state.wed,
                    mean.state.thu, mean.state.fri, mean.state.sat,
                    mean.state.sun)
Dmean.state = data.frame(State = D.state[, 1], Dmean.state = Dmean.state)
write.csv(Dmean.state, "data/Dmean.state.csv", row.names = FALSE)

# tmp = as.matrix(I.state[, -1])
# tmp = as.matrix(I.state[, -1]) - as.matrix(cbind(0, I.state[, -c(1, (n.day + 1))]))
tmp = as.matrix(I.state[, -1]) - as.matrix(cbind(matrix(0, nrow = n.state, ncol = 7), I.state[, -c(1, (n.day + (-5:1)))]))
for(i in 1:n.state){
  filename = "plot/New_Infect_lagts_"
  title = "New Infected Count Lagged Time Seires:"
  state.show = state[i]
  count = as.vector(tmp[i,])
  dat = data.frame(date = date, day = day, count = count)
  plot.ts(state.show, dat, title, filename)
}

# tmp = as.matrix(D.state[, -1])
# tmp = as.matrix(D.state[, -1]) - as.matrix(cbind(0, D.state[, -c(1, (n.day + 1))]))
tmp = as.matrix(D.state[, -1]) - as.matrix(cbind(matrix(0, nrow = n.state, ncol = 7), I.state[, -c(1, (n.day + (-5:1)))]))
for(i in 1:n.state){
  filename = "plot/New_Death_lagts_"
  title = "New Death Count Lagged Time Seires:"
  state.show = state[i]
  count = as.vector(tmp[i,])
  dat = data.frame(date = date, day = day, count = count)
  plot.ts(state.show, dat, title, filename)
}

state = unique(I.county$State)
county = I.county$County
n.day = ncol(I.county) - 3
n.state = length(state)
n.county = length(county)
date = substring(names(I.county[, -(1:3)]), 2)
date = as.Date(gsub('\\.', '-', date))
day = weekdays(date)
for(i in 1:n.county){
  filename = "plot/infection_ts_"
  title = "Infected Count Time Seires:"
  state.show = I.county$State[i]
  county.show = I.county$County[i]
  loc.show = paste0(county.show, ", ", state.show)
  tmp = as.matrix(I.county[, -(1:3)])
  count = as.vector(tmp[i,])
  dat = data.frame(date = date, day = day, count = count)
  plot.ts(loc.show, dat, title, filename)
}

######################################################################
# Step 1. Order-Dependence Detection
res1.OD = detect.OD(I.county, "county")
dat.I.county = res1.OD$dat.new
res2.OD = detect.OD(D.county, "county")
dat.D.county = res2.OD$dat.new
# res3.OD = detect.OD(I.state, "state")
# dat.I.state = res3.OD$dat.new
# res4.OD = detect.OD(D.state, "state")
# dat.D.state = res4.OD$dat.new

dat.I = dat.I.county[apply(is.na(dat.I.county), 1, sum) < (ncol(dat.I.county) - 1),]
dat.D = dat.D.county[apply(is.na(dat.D.county), 1, sum) < (ncol(dat.D.county) - 1),]
dat.rep.county = repair.cdcar(dat.I = dat.I, dat.D = dat.D, h = 7, level = "county", method = "AR")
I.county = dat.rep.county$dat.rep.I
save(I.county, file = paste0("data/I_county_", as.character(date.update), ".rda"))
D.county = dat.rep.county$dat.rep.D
save(D.county, file = paste0("data/D_county_", as.character(date.update), ".rda"))

# dat.I = dat.I.state[apply(is.na(dat.I.state), 1, sum) < (ncol(dat.I.state) - 1),]
# dat.D = dat.D.state[apply(is.na(dat.D.state), 1, sum) < (ncol(dat.D.state) - 1),]
# dat.rep.state = repair.cdcar(dat.I = dat.I, dat.D = dat.D, h = 7, level = "state", method = "AR")
# I.state = dat.rep.state$dat.rep.I
# save(I.state, file = paste0("data/I_state_", as.character(date.update), ".rda"))
# D.state = dat.rep.state$dat.rep.D
# save(D.state, file = paste0("data/D_state_", as.character(date.update), ".rda"))

# ind = (1:nrow(dat.I))[apply(is.na(dat.I[, -1]), 1, sum) > 0]
# dat3.plot = res3.OD$dat.sub[ind,]
# plot.cdcar(dat3.plot, "state")
# dat4.plot = dat.rep$dat.rep.I[ind,]
# plot.cdcar(dat4.plot, "state")

I.county.JP = detect.JP(I.county,  window.s = 1, window.l = 9, trun.scale = 6, level = "county")
D.county.JP = detect.JP(D.county,  window.s = 1, window.l = 9, trun.scale = 6, level = "county")

######################################################################
# 2. Change Point detection
res3.CP = detect.CP(dat.rep$dat.rep.I, level = "state", manual.decision = TRUE)
res4.CP = detect.CP(dat.rep$dat.rep.D, level = "state", manual.decision = TRUE)
ind = (1:nrow(dat.I))[apply(is.na(res4.CP$dat.new), 1, sum) > 0]



######################################################################
# 3. Delay reporting detection
res3.DR = detect.DR(dat.rep$dat.rep.I, level = "state")
res4.DR = detect.DR(dat.rep$dat.rep.D, level = "state")
ind = (1:nrow(res3.DR$dat.sub))[res3.DR$dr.res[, 2] == 1]
dat3.plot = res3.DR$dat.sub[ind,]
plot.cdcar(dat3.plot, "state")
ind = (1:nrow(res4.DR$dat.sub))[res4.DR$dr.res[, 2] == 1]
dat4.plot = res4.DR$dat.sub[ind,]
plot.cdcar(dat4.plot, "state")
