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
geo_df = read.csv("data/geo_df.csv", stringsAsFactors = F)
states = geojsonio::geojson_read("data/br-states.json", what = "sp")
order_df = read.csv("data/final_order.csv", stringsAsFactors = F)
time_df = read.csv("data/time_df.csv", stringsAsFactors = F)
cat_df = read.csv("data/cat_df.csv", stringsAsFactors = F)
cat_time_df = read.csv("data/cat_time_df.csv", stringsAsFactors = F)


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

xcol_choices = list(
        "Average Order Value" = names(geo_df)[[3]],
        "Average Shipping Cost" = names(geo_df)[[4]],
        "Shipping Cost/Order Ratio" = names(geo_df)[[5]],
        "Average Delivery Days" = names(geo_df)[[6]]
)

ycol_choices = list(
        "Average Review Score" = names(geo_df)[[7]],
        "Est - Actual Deliver Time" = names(geo_df)[[8]]
)

trd_choices = sort(colnames(cat_time_df)[2:13])

catvalue_choices = list(
        "Total Sales" = names(cat_df)[[2]],
        "Unit Sales" = names(cat_df)[[3]],
        "Average Review Score" = names(cat_df)[[4]]
)

cats_choices = sort(cat_df$category)

intro_str =  "Marketing has always been an industry that is heavily data related. Big firms spent millions of dollars every year on analysising their marketing data, in terms of 
finding insights and make their marketing investment wisely. Because of my marketing background, discovering insights from a marketing dataset 
always insterest me, so I did this project that is using a Brazilian online retail marketplace's sales data to understand the Brazil's E-commerce from mutiple perspectives. This 
shiny app has three major sections. With geographic section, you could visualize data from a geographic standpoint and see some correlations. The trends section is the part to show
sales trends in the particular time range you selected, you could also comparing sales volume between categories. The categories section visualize the data from its different
categories perspective."

intrdata_str = "The dataset I am using is provided by the largest department store in Brazilian marketplaces, called Olist. It inculdes its over 100k orders from late 2016 to 2018.
The dataset's schema is showing below in the graphy. The data is divided into multiple datasets. It allows us to view the Brazilian E-commerce from mutiple demisions like order status,
payments information, geographic location and so on."

