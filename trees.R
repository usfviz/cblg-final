# ------------------------------------------------------- #
# LIBRARIES
# ------------------------------------------------------- #

# CHECK TO MAKE SURE PACKAGES ARE INSTALLED ------------- #
if (!require(RColorBrewer)){
  install.packages(RColorBrewer)
}

if (!require(broman)){
  install.packages(broman)
}

if (!require(GGally)){
  install.packages(GGally)
}

if (!require(leaflet)){
  install.packages(leaflet)
}


if (!require(rgdal)){
  install.packages('geos')
  install.packages('rgeos', type="source")
  install.packages('rgdal', type = "source")
}

# CALL LIBRARIES ---------------------------------------- #

library(rsconnect)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(broman)
library(magrittr)
library(reshape2)
library(ggvis)
library(lmtest)
library(MASS)
library(car)
library(boot)
library(caret)
library(Matrix)
library(knitr)
library(stats)
library(GGally)
library(leaflet)
library(shiny)
library(foreign)
library(sp)
library(rgdal)
library(rgeos)
library(lubridate)
library(zoo)
library(FNN)

# Zillow logo: https://www.zillowstatic.com/vstatic/3af70b5/static/logos/Zillow_Logo_HoodsProvided_RightAligned.gif

# ------------------------------------------------------- #
# IMPORT DATA
# ------------------------------------------------------- #

tree.list <-   read.csv("data/Street_Tree_List_Updated2.csv", stringsAsFactors = FALSE)
sf.hoods         <- readOGR("data/ZillowNeighborhoods-CA/ZillowNeighborhoods-CA.shp")
sf.hoods.only    <- sf.hoods[sf.hoods$City == 'San Francisco',]

# ------------------------------------------------------- #
# CLEAN DATA & MAKE NEW VARIABLES
# ------------------------------------------------------- #    

tree.list$PlantYrMo         <- as.Date(as.POSIXct(strptime(tree.list$PlantDate, format="%m/%d/%Y %H:%M:%S %p")), format = "%Y-%m-%d")
mday(tree.list$PlantYrMo)   <- 1
tree.list$PlantYrMo[is.na(tree.list$PlantYrMo)] <- as.Date("1955-09-01")
tree.list$Owner            <- ifelse(tree.list$qCaretaker == 'Private', "Private", "Public" )

# ------------------------------------------------------- #
# CUSTOM COLOR PALETTE
# ------------------------------------------------------- # 
tree_green <- c("#234D20", "#36802D", "#77AB59","#C9DF8A","#F0F7DA")
tree.green.pal = colorRampPalette(tree_green, space = "Lab")(n = length(unique(tree.list$family)))

# ------------------------------------------------------- #
# DATA AFTER 1990 FOR MAP ANIMATION
# ------------------------------------------------------- #
data.in <- tree.list[tree.list$PlantYrMo > "1990-01-01",]

# ------------------------------------------------------- #
# YEARS AND SPECIES DATA 
# ------------------------------------------------------- #

tree.list$count <- 1
all.types <- unique(tree.list$species.common)
years.and.species <- tree.list[c("species.common", "year", "count")]
padding <- data.frame(species.common = all.types, year = rep(1955, length(all.types)), count = rep(0, length(all.types)))
years.and.species <- rbind(years.and.species, padding)
years.and.species <- data.frame(na.omit(years.and.species))

species_summary <- function(df){
  summary_df <- data.frame(species.common = unique(df$species.common))
  summary_df$count_trees <- 0
  for (i in 1:nrow(summary_df)){
    summary_df[i,"count"] <- sum(df[df$species.common == summary_df[i,1],"count"])
  }
  return(summary_df)
}

yas.sum <- species_summary(years.and.species)
y_max <- max(yas.sum$count)


get_decade <- function(year){
  dec <- ''
  if (year < 1960){
    dec <- '50s'
  } else if (year < 1970){
    dec <- '60s'
  } else if (year < 1980){
    dec <- '70s'
  } else if (year < 1990){
    dec <- '80s'
  } else if (year < 2000){
    dec <- '90s'
  } else if (year < 2010){
    dec <- '00s'
  } else if (year < 2020){
    dec <- '10s'
  } else {
    dec <- 'Unk'
  }
  return(dec)
}

years.and.species <- years.and.species[order(years.and.species),]

has.year <- ifelse(is.na(years.and.species$year), FALSE, TRUE)

years.and.species <- years.and.species[has.year,]

years.and.species$decade <- sapply(years.and.species$year,get_decade)

years.and.species$decade <- factor(years.and.species$decade, levels = c('10s','00s','90s','80s','70s','60s','50s'))

# ------------------------------------------------------ #
# BUBBLE CHART
# ------------------------------------------------------ #


invasive  <- read.csv("data/Invasive_Species.csv", stringsAsFactors = FALSE)

# CENSUS BLOCK INFO ------------------------------------------------------ #
# used for latitude and longitude

census_blocks           <- read.csv("data/census_blocks.csv", stringsAsFactors = FALSE)
census_blocks           <- census_blocks[c("BLOCKCE10", "INTPTLAT10", "INTPTLON10")]
colnames(census_blocks) <- c("block_group", "Latitude", "Longitude")

# MEDIAN INCOME BY CENSUS BLOCK ------------------------------------------ #
income <- read.csv("data/census_incomes/income_data.csv", skip = 1, stringsAsFactors = FALSE)
income$Id <- NULL
income$Margin.of.Error..Median.household.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars. <- NULL
colnames(income)       <- c("Id", "Name", "Median.income")
income$Median.income   <- as.integer(income$Median.income)
income$block_group     <- substr(income$Id,8,11)

# POPULATION BY CENSUS BLOCK ------------------------------------------ #
pops <- read.csv("data/census_pops/pop.csv", skip = 1, stringsAsFactors = FALSE)
pops$Id                     <- NULL
pops$Margin.of.Error..Total <- NULL
colnames(pops)              <- c("Id", "Name", "Population")
pops$block_group            <- substr(pops$Id, 8, 11)

# JOIN INCOME AND POPULATION BY CENSUS BLOCK ------------------------------------------ #
income             <- base::merge(income, pops, by = c("Id", "block_group", "Name"), all.x = TRUE)
income$block_group <- as.numeric(income$block_group)

# JOIN INCOME AND CENSUS BLOCK ------------------------------------------ #
census_blocks$block_group   <- as.numeric(census_blocks$block_group)
income                      <- base::merge(income, census_blocks, by = 'block_group', all.x = TRUE)

income                     <- aggregate(income, 
                                        by = list(income$block_group, income$Id, income$Name, income$Median.income, income$Population),
                                        FUN = mean)

income[6:10]      <- NULL
colnames(income)  <- c("block_group", "Id", "Name", "Median.income", "Population", "Latitude", "Longitude")
income            <- na.omit(income)
income[1:3]       <- NULL

# Get census block neighborhood names by closest lat/long in tree data

k                 <- knn(train = tree.list[c("Latitude", "Longitude")], 
                         test = income[c("Latitude", "Longitude")], 
                         cl = tree.list$hood.names, 
                         k = 1)
income            <- cbind(income, "hood.names" = k)
income$hood.names <- as.character(income$hood.names)
indices           <- attr(k, "nn.index")
income$hood.area  <- tree.list[indices, "scaled.area"]

# Aggregate on neighborhood by mean (all except area, population)

income.agg <- aggregate(income[c(1,3,4)], by = list(income$hood.names), FUN = mean)
colnames(income.agg) <- c("Neighborhood", "Median.income", "Latitude", "Longitude")

# Aggregate on neighborhood area by max

area.agg <- aggregate(income$hood.area, by = list(income$hood.names), FUN = max)
colnames(area.agg) <- c("Neighborhood", "Area")

# Aggregate population by total

pop.agg <- aggregate(income$Population, by = list(income$hood.names), FUN = sum)
colnames(pop.agg) <- c("Neighborhood", "Population")

df.full <- merge(income.agg, pop.agg)
df.full <- merge(df.full, area.agg)

# Count trees, invasive trees by neighborhood

tree.list$count <- 1

tree.list$invasive <- tree.list$species.science %in% invasive$Species.Name

tree.agg <- aggregate(tree.list[,c("count", "invasive")], by = list(tree.list$hood.names), FUN = sum)

colnames(tree.agg) <- c("Neighborhood", "Tree.count", "Invasive.count")

# Merge demo info + tree info

df.full <- merge(df.full, tree.agg, by = "Neighborhood", all.x = TRUE, all.y = FALSE)

# Create stats

df.full$Invasive.pct  <- df.full$Invasive.count/df.full$Tree.count
df.full$Pop.density   <- df.full$Population/df.full$Area
df.full$Tree.density  <- df.full$Tree.count/df.full$Area

df.full$id <- 1:nrow(df.full)

# Plotting lists

x.axis.choices <- c("Income", "Area", "Population", "Tree count", "Population density", "Tree density")
y.axis.choices <- c("Income", "Area", "Population", "Tree count", "Population density", "Tree density")
vis.choices <- c("Income", "Area", "Population", "Tree count", "Percent Invasive")
bubble.labs <- list(Income = "Income", Area = "Area", Invasive.pct = "Percent Invasive", Population = "Population", Tree.count = "Tree count", Pop.density = "Population density", Tree.density = "Tree density")


