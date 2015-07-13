TeamData <- read.csv("/Users/Kyle_Mac/Downloads/NDrive/skku 최신/2015 1학기여름방학/data science/팀플/dataCombined2.csv")
ggplot(TeamData, aes(x=Year, y=Life.expectancy, group=Continent, colour=Continent))+
  geom_point(size=2, shape=15)
