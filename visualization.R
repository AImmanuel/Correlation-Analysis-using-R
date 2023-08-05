library(tidyverse)
#pop <- read_csv("https://data.world/data-literacy/world-bank-population-life-expectancy-urbanization/workspace/file?filename=populationlifeexpectancy.csv", col_names = FALSE)
pop <- read.csv("https://query.data.world/s/vij76cg43v634klakxokfhjw57ezvq", header=TRUE, stringsAsFactors=FALSE)

pop_10_17 <- pop[pop$Year %in% c('2010','2011','2012','2013','2014','2015','2016','2017'),]
pop_10_17<-na.omit(pop_10_17)

urban_pop_percentage<-as.numeric((pop_10_17$Urban.Population/pop_10_17$Population)*100)

#cor(urban_pop_percentage, as.numeric(pop_10_17$Life.Expectancy.in.Years),use = "pairwise.complete.obs")
pdf("visualization.pdf")

plot(urban_pop_percentage,pop_10_17$Life.Expectancy.in.Years,pch=1,xlab="Urban Population Percentage",ylab="Life Expectancy in Years",main="Urban Population Percentage v Life Expectancy in Years")
abline(lm(pop_10_17$Life.Expectancy.in.Years~urban_pop_percentage),col='blue')

#cor.test(urban_pop_percentage,pop_10_17$Life.Expectancy.in.Years,use="pairwise.complete.obs",method = 'pearson')
#hist(pop_10_17$Life.Expectancy.in.Years) #dependant variable

dt <- pop_10_17$Life.Expectancy.in.Years
dtMin = min(dt,na.rm=TRUE)
dtMax = max(dt,na.rm=TRUE)
dtMean = mean(dt,na.rm=TRUE)
dtSd = sd(dt,na.rm=TRUE)

h <- hist(dt, breaks = 9, density = 10,
          col = "lightgray", 
          ylab = "Frequency",
          xlab = "Countries' Average Life Expectancy in Years", 
          main = "Frequency Distribution of Life Expectancy (2010-2017)",
          xlim=c(dtMin,90),
          ylim=c(0,400)) 

x <- seq(dtMin, dtMax, .1)  
y1 <- dnorm(x, mean=dtMean, sd=dtSd) 

y1 <- y1 *diff(h$mids[1:2]) *length(dt) 
lines(x, y1, col="blue")
dev.off()