library(ggplot2)
library(readr)
library(dplyr)
library(shiny)
library(bslib)
library(shinydashboard)
library(fresh)
library(bcmaps)
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(plotly)
library(tidyr)
library(lubridate)
library(rsconnect)

source("R/data_helpers.R")

# All externally-sourced data below is read from data/ or data/cache/, which is
# populated by data_refresh.R on a schedule. The app itself never
# scrapes or calls an external API directly at startup - if a scheduled
# refresh fails for a dataset, its cache file is left untouched, so the
# app keeps running on the last successful pull (see
# data/cache/refresh_log.csv for a history of refresh attempts).

#DROUGHT MAP ----------------------------------------------------------------------------------
df_drought <- read_cache_rds("drought")

#HISTORICAL DROUGHT  --------------------------------------------------------------------------
df_drought_hist <- read_cache_csv("drought_hist")

#WILDFIRE LOCATIONS 2022-Recent Year ------------------------------------------------------------------------
df_wildfires <- read_cache_rds("wildfire")

#TEMPERATURE MIN, MAX, MEAN
df_temp <- read_cache_csv("temperature")

#THOMPSON OKANAGAN TOURISM REGION BOUNDS ------------------------------------------------------
df_TO_boundary <- st_read("data/thompson_okanagan_boundary.geojson", quiet = TRUE)