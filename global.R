library(shiny)
library(shinythemes)
library(shinyBS)
library(DT)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
# lapply(list.files(pattern="^cc4lite_launch_.*.\\.RData$"), load, envir=.GlobalEnv)
# caption <- 'Due to inter-annual variability and model uncertainty, these graphs are useful for examining a range of projected trends, but not for precise prediction. For more information regarding climate projections, please visit'
# dec.lab <- paste0(seq(2010, 2090, by=10), "s")
# 
# brks <- c(0, 1e4, 5e4, 1e5, 2.5e5, 5e5, 1e6)
# nb <- length(brks)
# cities.meta$PopClass <- cut(cities.meta$Population, breaks=brks, include.lowest=TRUE, labels=FALSE)
# cities.meta$PopClass[is.na(cities.meta$PopClass)] <- 1
# palfun <- colorFactor(palette=c("navy", "navy", "magenta4", "magenta4", "red", "red"), domain=1:(nb-1))

sites2map <- readRDS("sites2map.rds")
names(sites2map)[6] <- "location"
names(sites2map)[8] <- "lng"
sitesonly <- sites2map %>%
  select(location, lat, lng) %>%
  distinct() %>% as.data.frame()
 # mutate(type = "notsel")

spec.sites <- readRDS("spec.site.rds")
spec.sites$Year <- as.numeric(as.character(spec.sites$Year))
spec.trend <- readRDS("spec.trend.rds")

ann.counts <- readRDS("annualcounts.rds")

all.counts <- ann.counts %>% 
  group_by(CommonName, location) %>%
  summarise(GrandTotal = sum(TotalCount)) %>%
  data.frame()

siteocc <- readRDS("siteocc.rds")

gdd <- readRDS("gdd.rds")
gdd$year <- as.character(year(gdd$date))

gdd$MonthN <- as.numeric(format(as.Date(gdd$date),"%m")) # Month's number
gdd$Month  <- months(as.Date(gdd$date), abbreviate=TRUE) # Month's abbr.
gdd$Ordinal <- yday(gdd$date)

temperature <- readRDS("monthlyweather.rds")