# prepare data for shiny app



library(readr)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)

raw <- read_csv(file = "C:/Users/Tyson/Desktop/Box Sync/Ohio/data2012/data.trim.csv")[,-1]
raw <- raw %>% mutate(SiteID = formatC(SiteID.x, width = 3, format = "d", flag = "0"),
                      SiteDate = ymd(as.character(SiteDate)),
                      Year = year(SiteDate))
raw$CommonName[raw$CommonName == "Spring/Summer Azure"] <- "Azures"

surveys <- distinct(raw[, c("SeqID", "SiteID", "SiteDate", "Week")])

sites <- read_csv("C:/Users/Tyson/Desktop/Box Sync/Ohio/GIS/OHsites_reconciled.csv")
sites <- sites %>% mutate(SiteID = formatC(as.numeric(Name), width = 3, format = "d", flag = "0"))

# jitter coordinates slightly for overlapping transects
sites$lat <- sites$lat + rnorm(length(sites$lat), mean = 0, sd = 0.001)
sites$lon <- sites$lon + rnorm(length(sites$lon), mean = 0, sd = 0.001)


# create list of species to use
SpeciesNum <- raw %>%
  group_by(CommonName) %>%
  summarise(Present = length(Total)) %>%
  arrange(-Present) %>%
  data.frame()

SpeciesList <- SpeciesNum[-grep("Unidentified", SpeciesNum$CommonName, fixed = TRUE), ]
SpeciesList <- SpeciesList[-which(SpeciesList$CommonName == "None seen this day"), ]
SpeciesList$CommonName <- plyr::mapvalues(SpeciesList$CommonName, 
                                          from = "Spring/Summer Azure", to = "Azures")
dat <- list()
for (i in 1:nrow(SpeciesList)){
  # for (i in 1:5){
    
  species <- SpeciesList$CommonName[i]
  spdat <- raw %>% filter(CommonName == species) %>% select(CommonName, SeqID, SiteDate, Week, SiteID, Year, Total)
  test <- merge(surveys, spdat, by = c("SeqID", "SiteID", "SiteDate", "Week"), all.x = TRUE)
  test$Total <- plyr::mapvalues(test[, "Total"], from = NA, to = 0)
  test2 <- test %>%
    group_by(SiteID) %>%
    mutate(Occupancy = length(which(Total > 0))/length(Total)) %>%
    ungroup()
  out <- test2 %>%
    group_by(Year) %>%
    mutate(YearTotal = sum(Total)) # %>%
    # filter(YearTotal > 0)
  dat[[i]] <- out
}
allraw <- data.table::rbindlist(dat) #dplyr rbind_list has bug

rawcounts <- merge(allraw, sites, by = "SiteID")
saveRDS(rawcounts, "rawcounts.rds")
# saveRDS(rawcounts, "testcounts.rds")

############################################################
# 1. raw site counts to plot on map by species, week, and year

rawcounts <- readRDS('rawcounts.rds')

# summarise rawcounts by site/year/species for tab 1
counts <- rawcounts %>%
  group_by(CommonName, Description.x, Year) %>%
  summarise(TotalCount = sum(Total)) %>%
  rename(location = Description.x) %>% 
  data.frame()

saveRDS(counts, "annualcounts.rds")

# occupancy from rawcounts for each species x site
siteocc <- rawcounts %>%
  select(CommonName, Occupancy, Description.x, lat, lon) %>%
  distinct()

saveRDS(siteocc, "siteocc.rds")


############################################################
# 2. Population index for sites/regions/state over time

setwd('C:/Users/Tyson/REPO/Chap1-Bfly-Landuse-Climate/')
# get the 65 species I have population estimates for
allcounts_files <- list.files("RDSfiles", pattern = "allcounts")
species <- unlist(strsplit(allcounts_files, "allcounts", fixed = TRUE))[seq(2,130,2)]
species <- unlist(strsplit(species, "[.]"))[seq(1,129,2)]

allcounts <- list()
phen_all <- list()
Pops <- list()
RegIndex <- list()

for (i in 1:length(species)){
  sp <- species[i]
  allcounts[[i]] <-  cbind(sp, readRDS(file = paste("RDSfiles/allcounts", sp, ".rds", sep = "")))
  phen_all[[i]] <- cbind(sp, readRDS(file = paste("RDSfiles/phenology", sp, ".rds", sep = "")))
  Pops[[i]] <- cbind(sp, readRDS(file = paste("RDSfiles/popsites", sp, ".rds", sep = "")))
  RegIndex[[i]] <- cbind(sp, readRDS(file = paste("RDSfiles/popindex", sp, ".rds", sep = "")))
}

allspcounts <- data.table::rbindlist(allcounts)
allspphen <- data.table::rbindlist(phen_all)
allspindex <- data.table::rbindlist(Pops)
allspregindex <- data.table::rbindlist(RegIndex)

setwd('../ohio_butterfly_dataviz')

saveRDS(allspphen, "phenology.rds")

# Need one df of species by site population size
# one df of statewide collated index by species
# how to scale on same plot?
spec.site <- as.data.frame(allspindex)
spec.site <- merge(spec.site, sites, by = "SiteID", all.x = TRUE, all.y = FALSE)
names(spec.site)[2] <- "CommonName"
names(spec.site)[9] <- "location"
names(spec.site)[11] <- "lng"
saveRDS(spec.site, "spec.site.rds")

spec.trend <- as.data.frame(allspregindex)
spec.trend <- spec.trend[spec.trend$Region == "ALL", ]
names(spec.trend)[1] <- "CommonName"
saveRDS(spec.trend, "spec.trend.rds")

############################################################
# Weather and phenology at sites/statewide
gdd <- readRDS("C:/Users/Tyson/Desktop/Box Sync/Ohio/daymet/growingDD_Daymet.RDS")
weather <- readRDS('C:/Users/Tyson/REPO/Chap1-Bfly-Landuse-Climate/data/siteweathervars.rds')
phenology <- readRDS('phenology.rds')
rawcounts <- readRDS('rawcounts.rds')

# weather data to plot mean seasonal temperatures
weather1 <- weather[, grep("meanTemp", names(weather)), with = FALSE]
weather1$SiteID <- weather$site
weather1$Year <- weather$year
weather <- weather1 %>%
  mutate(SiteID = formatC(as.numeric(as.character(SiteID)), width = 3, format = "d", flag = "0"))
weather <- merge(weather, sites, by = "SiteID")


# test plot
plotdat <- weather %>%
  filter(SiteID == "001") %>%
  gather(Season, Temperature, prevspr_meanTemp:currsum_meanTemp) %>%
  filter(Season %in% c("winter_meanTemp", "spring_meanTemp", "currsum_meanTemp"))
t <- ggplot(data = plotdat, aes(x = Season, y = Temperature, group = Year))
t + geom_line()

gdd <- gdd %>% data.frame() %>% filter(year >= 1995)
test <- as.POSIXct(strptime(paste(gdd$year, gdd$yday, sep = " "), format = "%Y %j"))
gdd$date <- test
gdd$month <- month(gdd$date, label = TRUE)

gdd$SiteID <- formatC(as.numeric(gdd$site), width = 3, format = "d", flag = "0")
gdd$dailymean <- (gdd$tmax..deg.c. + gdd$tmin..deg.c.)/2

monthmeans <- gdd %>%
  group_by(SiteID, year, month) %>%
  summarise(MeanTemperature = mean(dailymean))

monthmeans <- monthmeans %>%
  group_by(SiteID, month) %>%
  mutate(MeanAllYears = mean(MeanTemperature)) %>%
  mutate(Temperature_Anomaly = MeanTemperature - MeanAllYears)
monthmeans <- merge(monthmeans, sites, by = "SiteID")

gdd <- gdd %>%
  dplyr::select(SiteID, date, cumdegday)

gdd <- merge(gdd, sites, by = "SiteID")

saveRDS(gdd, "gdd.rds")
saveRDS(monthmeans, "monthlyweather.rds")

#############################################################
# 3. Species richness at sites in different years and weeks


sites2map <- rawcounts %>%
  filter(Total > 0) %>%
  group_by(SiteID, Year, Week) %>%
  mutate(surv.richness = length(unique(CommonName)),
         surv.total.counted = sum(Total)) %>%
  mutate(year.richness = length(unique(CommonName)),
            year.total.counted = sum(Total)) %>%
  mutate(grand.richness = length(unique(CommonName)),
         years.surved = length(unique(Year)),
         grand.total.counted = sum(Total)) %>%
  select(-CommonName, -Total, -Occupancy, -YearTotal, -Name)

sites2map <- sites2map[-which(duplicated(sites2map$SeqID)), ]

sites2map <- gather(sites2map, variable, value, surv.richness:grand.total.counted)
saveRDS(sites2map, "sites2map.rds")

