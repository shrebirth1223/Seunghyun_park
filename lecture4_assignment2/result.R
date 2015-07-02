
#clean variables
rm(list=ls(all=TRUE))

# read in data
P <- read.csv("/Users/Kyle_Mac/GitHub/Seunghyun_park/lecture4_assignment2/UNdata_Export_20150702_111245737.csv")
plot(P$Country.or.Area,P$Year)
