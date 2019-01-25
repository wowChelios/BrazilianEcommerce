library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(leaflet)
library(googleVis)
library(maps)
library(geojsonio)
library(RColorBrewer)

# loading dfs
geodf = read.csv("gdf.csv", stringsAsFactors = F)
states = geojsonio::geojson_read("br-states.json", what = "sp")
maindf = read.csv("mdf.csv", stringsAsFactors = F)
timdf = read.csv("tdf.csv", stringsAsFactors = F)
catdf = read.csv("cdf.csv", stringsAsFactors = F)
geochoices = colnames(geodf)[-1]

# Assigning variables
sbin = c(0, 100000, 200000, 300000, 400000, 1000000, 2000000, 5000000, Inf)
tbin = list(sbin, 1)
ccc= aaa == "sales"
tbin[[1]]
ifelse(aaa == "sales", tbin[[1]], 9)
?colorBin
