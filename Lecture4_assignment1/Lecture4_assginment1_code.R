# SKKU ISS 2015, Lecture 4
# Objective: plotting open government data

# Jevin West
# Date:  7/2/2015

#clean variables
rm(list=ls(all=TRUE))

# read in data
P <- read.csv("/Users/Kyle_Mac/GitHub/SKKU_DataScience_2015/data_sets/UNdata_precipitation.csv")

# (1) Compare the precipitation from 1999 to 2005.  Are they different?

#tapply(P$Value, P$Year[1999:2005], mean)

y1999 <- P[P$Year==1999,]
y2005 <- P[P$Year==2005,]

hist(y1999$Value)
hist(y2005$Value)

t.test(log10(y1999$Value),log10(y2005$Value))
# (2) Precipitation for all countries over time.
meantest2 <-  tapply(P$Value, P$Year, mean)
plot(c(1990,1995:2009),meantest2)

