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
  out <- test %>%
    group_by(Year) %>%
    mutate(YearTotal = sum(Total)) %>%
    filter(YearTotal > 0)
  dat[[i]] <- out
}
allraw <- data.table::rbindlist(dat) #dplyr rbind_list has bug

rawcounts <- merge(allraw, sites, by = "SiteID")
saveRDS(rawcounts, "rawcounts.rds")
# saveRDS(rawcounts, "testcounts.rds")

############################################################
# 1. raw site counts to plot on map by species, week, and year



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

#############################################################
# 3. Species richness at sites in different years and weeks

rawcounts <- readRDS('rawcounts.rds')

sites2map <- rawcounts %>%
  group_by(SiteID, Year, Week) %>%
  mutate(surv.richness = length(unique(CommonName)),
         surv.total.counted = sum(Total)) %>%
  mutate(year.richness = length(unique(CommonName)),
            year.total.counted = sum(Total)) %>%
  mutate(grand.richness = length(unique(CommonName)),
         years.surved = length(unique(Year)),
         grand.total.counted = sum(Total)) %>%
  select(-CommonName, -Total, -YearTotal, -Name)

# sites2map <- sites2map[-which(duplicated(sites2map$SeqID)), ]
# saveRDS(sites2map, "sites2map.rds")

sites2map <- gather(sites2map, variable, value, surv.richness:grand.total.counted)
saveRDS(sites2map, "sites2map.rds")
