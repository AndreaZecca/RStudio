### The importance of data visualization!

#install.packages("datasauRus")
# load needed packages
library(ggplot2)
library(datasauRus)

# let us consult the help for the dataset inside datasauRus
?datasaurus_dozen

# 13 datasets with same mean and sd:
aggregate(datasaurus_dozen$x, list(datasaurus_dozen$dataset), mean) # mean var x for each dataset
aggregate(datasaurus_dozen$y, list(datasaurus_dozen$dataset), mean) # mean var y
aggregate(datasaurus_dozen$y, list(datasaurus_dozen$dataset), sd)  # sd for y etc...

# and the same (low) correlation!
# oh, btw have a look at
browseURL("http://www.tylervigen.com/spurious-correlations")

# let's come back to our example:
dataset <- unique(datasaurus_dozen$dataset)

for(i in dataset){
  print(paste(i, 
              round(cor(datasaurus_dozen$x[datasaurus_dozen$dataset==i],
                        datasaurus_dozen$y[datasaurus_dozen$dataset==i]),3)))
}

# let's have a look...
windows()
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)


# to create some dynamic plot, a gif....

require(gifski)
require(png)
library(gganimate)

saurus <- ggplot(datasaurus_dozen, aes(x = x, y = y, frame = dataset)) +
  geom_point() +  theme(legend.position = "none") +  transition_manual(dataset )

# to save as a gif
animate(saurus, renderer = gifski_renderer(file = "dino-data.gif", loop = TRUE,
                                           width = NULL, height = NULL))


# A more "current" example ####
#be sure that all needed packages are loaded
require(ggplot2)
require(gifski)
require(png)
library(gganimate)
require(jsonlite)

# download updated data: nazional, regional and by province
dt_naz <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-andamento-nazionale.json")
dt_pro <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json")
dt_reg <- jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json")

### national

# working with dates
day <- 1:nrow(dt_naz)
xd1 <- as.Date("2020-02-24")
xd2 <- xd1 + 1:max(day)
xd <- substr(xd2, 6, 10)

# create a data.frame, recall that ggplot works ONLY with data.frame!
dt_n <- data.frame(day, value = dt_naz$totale_casi)

# plot for national data
covid <- ggplot(
  dt_n, # data to be used
  aes(day, value) # aesthetic setup: x = days, y = values of total cases
) +
  geom_line(col = "blue") + # draw a line of color blue
  labs(x = "Days", y = "Total") + # change axis labels
  scale_x_continuous(breaks = seq(1,max(day), 10), labels = xd[seq(1,max(day), 10)])+ # use days as labels for x-axis ticks
  theme(axis.text.x = element_text(size=11, angle=45))

covid  + transition_reveal(day) + geom_point(aes(group = seq_along(day)))

### region ####
# let us do the same as before, let us create a regional data.frame
dt_r <- data.frame(region = dt_reg$denominazione_regione, day = rep(day, each = 21), value = dt_reg$totale_casi)

covid_reg <- ggplot(
  dt_r,
  aes(day, value, group = region, color = region)
) +
  geom_line() +
  scale_color_viridis_d() + # use the "viridis" scale color
  labs(x = "Days", y = "") +
  theme(legend.position = "bottom")+theme(legend.title=element_blank(), axis.text.x = element_text(size=11, angle=45))+
  scale_x_continuous(breaks = seq(1,max(day), 10), labels = xd[seq(1,max(day), 10)]) # use days as labels for x-axis ticks


# let us create a dynamic plot with regional data
cov_reg <- covid_reg  + transition_reveal(day) + geom_point(aes(group = seq_along(day)))

# and save it as a gif
animate(cov_reg, renderer = gifski_renderer(file = "covid_reg.gif", loop = TRUE,
                                            width = NULL, height = NULL))




