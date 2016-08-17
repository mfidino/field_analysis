##################################
#
#
# Analysis script for egg laying
# project
#
# Written by Mason Fidino
# 8/17/2016
#
#
#

# get only the species with more than 10 observations

library(lme4)

to_keep <- nobs$species[nobs$nobs>9]

fa <- bd[which(bd$species %in% to_keep),]

fa$year <- as.numeric(fa$year) - (min(as.numeric((fa$year))+1))
fa$jdate <- as.numeric(fa$jdate)
fa <- fa[complete.cases(fa),]
fa$species <- factor(fa$species)

m1 <- lmer(jdate ~ year + (1+year|species), data = fa )

grps <- read.csv("mig_type.csv", header = TRUE)

library(dplyr)

a <- left_join(fa, grps, by = "species")
a$migtype <- factor(a$migtype, levels = c("resident", "long", "short"))
m2 <- lmer(jdate ~ year + migtype + (1+year|species), data = a)
