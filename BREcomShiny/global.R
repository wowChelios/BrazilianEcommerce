setwd('~/Desktop/QifanWorkspace/R/BrazilianEcommerce/BREcomShiny')
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(leaflet)
library(googleVis)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)

# loading dfs
geo_df = read.csv("geo_df.csv", stringsAsFactors = F)
states = geojsonio::geojson_read("br-states.json", what = "sp")
order_df = read.csv("final_order.csv", stringsAsFactors = F)
time_df = read.csv("time_df.csv", stringsAsFactors = F)
cat_df = read.csv("cat_df.csv", stringsAsFactors = F)
geo_choices = colnames(geo_df)[-1]

# Assigning variables

