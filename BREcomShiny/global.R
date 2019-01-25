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
library(shinyWidgets)

# loading dfs
geo_df = read.csv("geo_df.csv", stringsAsFactors = F)
states = geojsonio::geojson_read("br-states.json", what = "sp")
order_df = read.csv("final_order.csv", stringsAsFactors = F)
time_df = read.csv("time_df.csv", stringsAsFactors = F)
cat_df = read.csv("cat_df.csv", stringsAsFactors = F)
cat_time_df = read.csv("cat_time_df.csv", stringsAsFactors = F)

# Assigning variables
geo_choices = list(
        "Total Sales" = names(geo_df)[[2]],
        "Average Order Value" = names(geo_df)[[3]],
        "Average Shipping Cost" = names(geo_df)[[4]],
        "Shipping Cost/Order Ratio" = names(geo_df)[[5]],
        "Average Delivery Days" = names(geo_df)[[6]],
        "Average Review Score" = names(geo_df)[[7]],
        "Est - Actual Deliver Time" = names(geo_df)[[8]]
)

catvalue_choices = list(
        "Total Sales" = "total_sales",
        "Unit Sales" = "salesperitem",
        "Average Review Score" = "avg_review"
)

cats_choices = sort(cat_df$product_category)

sort(cats_choices)
class(cats_choices)
sort(cat_df$product_category)

