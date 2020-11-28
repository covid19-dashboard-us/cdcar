rm(list = ls())

# Packages
library(cdcar)
library(dplyr)
library(mgcv)
library(plotly)
library(seastests)
library(segmented)
library(tscount)

date.update = as.Date("2020-11-27")

######################################################################
# Step 0. Read in Data
source.county = c("NYT", "JHU", "USAFacts")
source.state = c("Atlantic", "NYT", "JHU", "USAFacts")

# Compare infection state data
for (i.s in source.county[2]){
  I.county <- read.delim(paste0("data/", i.s, "_all_Infected_county_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  D.county <- read.delim(paste0("data/", i.s, "_all_Death_county_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  I.county = eval(parse(text = paste0("I.county.", i.s, " = I.county[,c(1:3, ncol(I.county):4)]")))
  D.county = eval(parse(text = paste0("D.county.", i.s, " = D.county[,c(1:3, ncol(I.county):4)]")))
  iI = (1:nrow(I.county))[apply(is.na(I.county), 1, sum) == (ncol(I.county) - 3)]
  I.county = I.county[-iI, ]
  iD = (1:nrow(D.county))[apply(is.na(D.county), 1, sum) == (ncol(D.county) - 3)]
  D.county = D.county[-iD, ]
}

for (i.s in source.state[3]){
  I.state <- read.delim(paste0("data/", i.s, "_all_Infected_state_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  D.state <- read.delim(paste0("data/", i.s, "_all_Death_state_", as.character(date.update), "_updated_original.tsv"), header = TRUE)
  I.state = eval(parse(text = paste0("I.state.", i.s, " = I.state[,c(1, ncol(I.state):2)]")))
  D.state = eval(parse(text = paste0("D.state.", i.s, " = D.state[,c(1, ncol(D.state):2)]")))
  iI = (1:nrow(I.state))[apply(is.na(I.state), 1, sum) == (ncol(I.state) - 1)]
  if(length(iI) > 0){
    I.state = I.state[-iI, ]
  }
  iD = (1:nrow(D.state))[apply(is.na(D.state), 1, sum) == (ncol(D.state) - 1)]
  if(length(iD) > 0){
    D.state = D.state[-iD, ]
  }
}

state = unique(I.state$State)
n.day = ncol(I.state) - 1
n.state = length(state)
date = substring(names(I.state[, -1]), 2)
date = as.Date(gsub('\\.', '-', date))
day = weekdays(date)

######################################################################
# Step 1. Order-Dependence Detection
res1.OD = detect.OD(I.county, "county")
dat.I.county = res1.OD$dat.new
sum(is.na(dat.I.county))
res2.OD = detect.OD(D.county, "county")
dat.D.county = res2.OD$dat.new
sum(is.na(dat.D.county))
res3.OD = detect.OD(I.state, "state")
dat.I.state = res3.OD$dat.new
sum(is.na(dat.I.state))
res4.OD = detect.OD(D.state, "state")
dat.D.state = res4.OD$dat.new
sum(is.na(dat.D.state))

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
save(I.state, file = paste0("data/", i.s, "_I_state_", as.character(date.update), ".rda"))
D.state = dat.rep.state$dat.rep.D
save(D.state, file = paste0("data/", i.s, "_D_state_", as.character(date.update), ".rda"))

##########
var.county.o = names(I.county)
test = I.county[, -(1:3)] - D.county[, -(1:3)]
sum(test < 0)
ind = which(test < 0, arr.ind = TRUE)
for(i in 1:nrow(ind)){
  indi = ind[i, ]
  indi[2] = indi[2] + 3
  cat("Now working on:", indi, "\n")
  cat("Before: D =", D.county[indi[1], indi[2]], "I =", I.county[indi[1], indi[2]])
  D.county[indi[1], indi[2]] = I.county[indi[1], indi[2]]
  cat(" After: D =", D.county[indi[1], indi[2]], "I =", I.county[indi[1], indi[2]], "\n")
}
test = I.county[, -(1:3)] - D.county[, -(1:3)]
sum(test < 0)

I.county.JP = detect.JP(dat = I.county, window = 14, level = "county")
I.county.JP = I.county.JP$dat.new
D.county.JP = detect.JP(dat = D.county, window = 14, level = "county")
D.county.JP = D.county.JP$dat.new
sum(is.na(I.county.JP))
sum(is.na(D.county.JP))

dat.county.JPrep = repair.JP(dat.I.JP = I.county.JP, dat.D.JP = D.county.JP, dat.I = I.county, dat.D = D.county, level = "county")
I.county.JPrep = dat.county.JPrep$dat.rep.I
I.county.JPrep = eval(parse(text = paste0("I.county.JPrep", " = I.county.JPrep[,c(1:3, ncol(I.county.JPrep):4)]")))
names(I.county.JPrep) = var.county.o
file.name = paste0("data/", i.s, "_Cum_Infected_county_", as.character(date.update), "_updated.tsv")
write.table(I.county.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)

D.county.JPrep = dat.county.JPrep$dat.rep.D
D.county.JPrep = eval(parse(text = paste0("D.county.JPrep", " = D.county.JPrep[,c(1:3, ncol(D.county.JPrep):4)]")))
names(D.county.JPrep) = var.county.o
file.name = paste0("data/", i.s, "_Cum_Death_county_", as.character(date.update), "_updated.tsv")
write.table(D.county.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)

######################################################################
var.state.o = names(I.state)
test = I.state[, -1] - D.state[, -1]
sum(test < 0)

I.JP = detect.JP(dat = I.state, window = 14, level = "state")
D.JP = detect.JP(dat = D.state, window = 14, level = "state")
I.state.JP = I.JP$dat.new
D.state.JP = D.JP$dat.new
sum(is.na(I.state.JP))
sum(is.na(D.state.JP))

dat.state.JPrep = repair.JP(dat.I.JP = I.state.JP, dat.D.JP = D.state.JP, dat.I = I.state, dat.D = D.state, level = "state")
I.state.JPrep = dat.state.JPrep$dat.rep.I
I.state.JPrep = eval(parse(text = paste0("I.state.JPrep", " = I.state.JPrep[,c(1, ncol(I.state.JPrep):2)]")))
names(I.state.JPrep) = var.state.o
file.name = paste0("data/", i.s, "_Cum_Infection_state_", as.character(date.update), "_updated.tsv")
write.table(I.state.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)

D.state.JPrep = dat.state.JPrep$dat.rep.D
D.state.JPrep = eval(parse(text = paste0("D.state.JPrep", " = D.state.JPrep[,c(1, ncol(D.state.JPrep):2)]")))
names(D.state.JPrep) = var.state.o
file.name = paste0("data/", i.s, "_Cum_Death_state_", as.character(date.update), "_updated.tsv")
write.table(D.state.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)
