rm(list = ls())

# Packages
library(cdcar)
library(dplyr)
library(mgcv)
library(plotly)
library(seastests)
library(segmented)
library(tscount)

date.update = as.Date("2020-08-08")
I.county <- read.delim(paste0("data/JHU_Cum_Infected_county_", as.character(date.update), "_updated.tsv"), header = TRUE)
D.county <- read.delim(paste0("data/JHU_Cum_Death_county_", as.character(date.update), "_updated.tsv"), header = TRUE)
var.county.o = names(I.county)
I.county = eval(parse(text = paste0("I.county", " = I.county[,c(1:3, ncol(I.county):4)]")))
D.county = eval(parse(text = paste0("D.county", " = D.county[,c(1:3, ncol(D.county):4)]")))
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
file.name = paste0("data/Cum_Infected_county_", as.character(date.update), "_updated.tsv")
write.table(I.county.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)

D.county.JPrep = dat.county.JPrep$dat.rep.D
D.county.JPrep = eval(parse(text = paste0("D.county.JPrep", " = D.county.JPrep[,c(1:3, ncol(D.county.JPrep):4)]")))
names(D.county.JPrep) = var.county.o
file.name = paste0("data/Cum_Death_county_", as.character(date.update), "_updated.tsv")
write.table(D.county.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)

######################################################################
I.state <- read.delim(paste0("data/JHU_Cum_Infected_state_", as.character(date.update), "_updated.tsv"), header = TRUE)
D.state <- read.delim(paste0("data/JHU_Cum_Death_state_", as.character(date.update), "_updated.tsv"), header = TRUE)
var.state.o = names(I.state)
I.state = eval(parse(text = paste0("I.state", " = I.state[,c(1, ncol(I.state):2)]")))
D.state = eval(parse(text = paste0("D.state", " = D.state[,c(1, ncol(D.state):2)]")))
test = I.state[, -1] - D.state[, -1]
sum(test < 0)

I.JP = detect.JP(dat = I.state, window = 14, level = "state")
D.JP = detect.JP(dat = D.state, window = 14, level = "state")
I.state.JP = I.JP$dat.new
D.state.JP = D.JP$dat.new
sum(is.na(I.state.JP))
sum(is.na(D.state.JP))

dat.state.JPrep = repair.JP(dat.I.JP = I.state.JP, dat.D.JP = D.state.JP, dat.I = I.state, dat.D = D.state, level = "state")
D.state.JPrep = dat.state.JPrep$dat.rep.D
D.state.JPrep = eval(parse(text = paste0("D.state.JPrep", " = D.state.JPrep[,c(1, ncol(D.state.JPrep):2)]")))
names(D.state.JPrep) = var.state.o
file.name = paste0("data/Cum_Death_state_", as.character(date.update), "_updated.tsv")
write.table(D.state.JPrep, file = file.name, quote = FALSE, sep = "\t", row.names = FALSE)
