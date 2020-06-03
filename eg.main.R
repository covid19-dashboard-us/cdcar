rm(list = ls())

# Packages
library(cdcar)
library(dplyr)
library(mgcv)
library(plotly)
library(seastests)
library(segmented)
library(tscount)

date.update = as.Date("2020-06-01")
######################################################################
# Step 0. ReadinData Readin
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

######################################################################
# Step 1. Order-Dependence Detection
res1.OD = detect.OD(I.county, "county")
dat.I.county = res1.OD$dat.new
res2.OD = detect.OD(D.county, "county")
dat.D.county = res2.OD$dat.new
res3.OD = detect.OD(I.state, "state")
dat.I.state = res3.OD$dat.new
res4.OD = detect.OD(D.state, "state")
dat.D.state = res4.OD$dat.new

dat.I = dat.I.county[apply(is.na(dat.I.county), 1, sum) < (ncol(dat.I.county) - 1),]
dat.D = dat.D.county[apply(is.na(dat.D.county), 1, sum) < (ncol(dat.D.county) - 1),]
dat.rep.county = repair.cdcar(dat.I = dat.I, dat.D = dat.D, h = 7, level = "county", method = "AR")
I.county = dat.rep.county$dat.rep.I
save(I.county, file = paste0("data/I_county_", as.character(date.update), ".rda"))
D.county = dat.rep.county$dat.rep.D
save(D.county, file = paste0("data/D_county_", as.character(date.update), ".rda"))

dat.I = dat.I.state[apply(is.na(dat.I.state), 1, sum) < (ncol(dat.I.state) - 1),]
dat.D = dat.D.state[apply(is.na(dat.D.state), 1, sum) < (ncol(dat.D.state) - 1),]
dat.rep.state = repair.cdcar(dat.I = dat.I, dat.D = dat.D, h = 7, level = "state", method = "AR")
I.state = dat.rep.state$dat.rep.I
save(I.state, file = paste0("data/I_state_", as.character(date.update), ".rda"))
D.state = dat.rep.state$dat.rep.D
save(D.state, file = paste0("data/D_state_", as.character(date.update), ".rda"))

write.csv(I.county, paste0("data/I_county_", as.character(date.update), ".csv"), row.names = FALSE)
write.csv(I.state, paste0("data/I_state_", as.character(date.update), ".csv"), row.names = FALSE)
write.csv(D.county, paste0("data/D_county_", as.character(date.update), ".csv"), row.names = FALSE)
write.csv(D.state, paste0("data/D_state_", as.character(date.update), ".csv"), row.names = FALSE)

ind = (1:nrow(dat.I))[apply(is.na(dat.I[, -1]), 1, sum) > 0]
dat3.plot = res3.OD$dat.sub[ind,]
plot.cdcar(dat3.plot, "state")
dat4.plot = dat.rep$dat.rep.I[ind,]
plot.cdcar(dat4.plot, "state")

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
