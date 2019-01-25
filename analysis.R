getwd()
setwd('..')
setwd('../Desktop/Qifanworkspace/R/BrazilianEcommerce')
library(tidyverse)
library(googleVis)
library(leaflet)
library(maps)
library(datasets)
install.packages("geojsonio")
states = geojsonio::geojson_read("json/br-states.json", what = "sp")
names(states)
reviews = read.csv('brazilian-ecommerce/olist_order_reviews_dataset.csv', stringsAsFactors = F)
payments = read.csv('brazilian-ecommerce/olist_order_payments_dataset.csv', stringsAsFactors = F)
orders = read.csv('brazilian-ecommerce/olist_orders_dataset.csv', stringsAsFactors = F)
customer = read.csv('brazilian-ecommerce/olist_customers_dataset.csv', stringsAsFactors = F)
products = read.csv('brazilian-ecommerce/olist_products_dataset.csv', stringsAsFactors = F)
items = read.csv('brazilian-ecommerce/olist_order_items_dataset.csv', stringsAsFactors = F)
sellers = read.csv('brazilian-ecommerce/olist_sellers_dataset.csv', stringsAsFactors = F)
geolocation = read.csv('brazilian-ecommerce/olist_geolocation_dataset.csv', stringsAsFactors = F)
categorynames = read.csv('brazilian-ecommerce/product_category_name_translation.csv', stringsAsFactors = F)

#Comparing prices from orders & payments
spfi = items %>%
        group_by(order_id) %>%
        summarise(sumprice = sum(price))

spfo = orders %>%
        left_join(payments, by = 'order_id') %>%
        select(order_id, payment_value) %>%
        group_by(order_id) %>%
        summarise(sumpricefo = sum(payment_value))

pricecheck = spfo %>%
        right_join(spfi, by = 'order_id') %>%
        mutate(shippingcost = sumpricefo - sumprice)

#Sales volume over times
SalesByTime = orders %>%
        filter(order_status != 'canceled') %>%
        select(order_id) %>%
        left_join(payments, by = 'order_id') %>%
        group_by(order_id) %>%
        summarise(sales = sum(payment_value)) %>%
        left_join(orders, by = 'order_id') %>%
        select(order_id, sales, order_purchase_timestamp) %>%
        filter(is.na(sales) == F) %>%
        mutate(purchase_date = substr(order_purchase_timestamp, 1, 10)) %>%
        mutate(week = (year(purchase_date) - year(min(purchase_date)))*52 + 
                        week(purchase_date) - week(min(purchase_date))) %>%
        arrange(purchase_date) %>%
        group_by(week) %>%
        summarise(totalsales = sum(sales))

g = ggplot(SalesByTime, aes(week, totalsales))
g + geom_line()

# Category analysis
productnames = products %>%
        select(product_id, product_category_name) %>%
        left_join(categorynames, by = 'product_category_name') %>%
        select(-2)

SalesByCategory = items %>%
        left_join(productnames, by = 'product_id') %>%
        group_by(product_category_name_english) %>%
        summarise(revenue = sum(price)) %>%
        arrange(desc(revenue))

c = ggplot(SalesByCategory, aes(x = "", revenue, fill = product_category_name_english))
c + geom_bar(width = 1, stat = 'identity')
c + coord_polar("y", start = 0)

# Cleaning Geographical data
c2 = customer %>%
        group_by(customer_zip_code_prefix) %>%
        summarise(n = n())



g2 = geolocation %>%
        distinct(geolocation_zip_code_prefix)
summary(g2)
summary(g1$geolocation_zip_code_prefix)
g1 = geolocation %>%
        select(geolocation_zip_code_prefix, geolocation_lat, geolocation_lng) %>%
        group_by(geolocation_zip_code_prefix) %>%
        summarise(lat = mean(geolocation_lat), lng = mean(geolocation_lng))

# Cleaning Payment Table
p1 = payments %>%
        group_by(order_id) %>%
        summarise(value = sum(payment_value))

i1 = items %>%
        group_by(order_id) %>%
        summarise(shipping_cost = sum(freight_value))

# Joining all tables
o1 = orders %>%
        left_join(p1, by = "order_id") %>%
        left_join(customer, by = "customer_id") %>%
        left_join(g1, by = c("customer_zip_code_prefix" = "geolocation_zip_code_prefix")) %>%
        left_join(reviews, by = "order_id") %>%
        left_join(i1, by = "order_id") %>%
        left_join(productnames, by = "product_id") %>%
        select(order_id, customer_id, order_status,order_purchase_timestamp, order_delivered_customer_date, 
                order_estimated_delivery_date, value, customer_unique_id, customer_zip_code_prefix, lat, lng, 
               customer_city, customer_state, review_score, product_id, product_category_name_english, shipping_cost)

# Cleaning Gaint Table Data
o2 = o1 %>%
        filter(order_status != "canceled" & value > 0) %>%
        rename(purchase_date = order_purchase_timestamp, delivered_date = order_delivered_customer_date, 
               est_delivered_date = order_estimated_delivery_date,zip = customer_zip_code_prefix, 
               city = customer_city, state = customer_state, product_category = product_category_name_english) %>%
        mutate(purchase_date = lubridate::as_datetime(purchase_date), 
               delivered_date = lubridate::as_datetime(delivered_date),
               est_delivered_date = lubridate::as_datetime(est_delivered_date))

o3 = o2 %>%
        mutate(purchase_date = lubridate::date(purchase_date), 
               delivered_date = lubridate::date(delivered_date),
               est_delivered_date = lubridate::date(est_delivered_date)) %>%
        mutate(delivery_days = delivered_date - purchase_date,
               diff_estdel = est_delivered_date - delivered_date)



# Generating analysis by states
revbystate = o2 %>%
        group_by(state) %>%
        summarise(totalrev = sum(value))

s1 = o3 %>%
        na.omit(shipping_cost, delivery_days, review_score, diff_estdel) %>%
        group_by(state) %>%
        summarise(sales = sum(value),
                  salesperorder = round(sum(value)/n(),2), 
                  avg_shipcost = round(mean(shipping_cost), 2),
                  avg_shcsratio = round(mean(shipping_cost/value),2),
                  avg_delidays = round(mean(delivery_days), 2),
                  avg_review = round(mean(review_score), 3),
                  avg_diffestdel = round(mean(diff_estdel),2))

write.csv(s1, file = "gdf.csv", row.names = F)

# Generating analysis by time
t1 = o3 %>%
        na.omit(purchase_date) %>%
        group_by(purchase_date) %>%
        summarise(salesbyday = sum(value))

t2 = o3 %>%
        na.omit(purchase_date) %>%
        filter(product_category == "pet_shop" & purchase_date >= "2018-01-01" & purchase_date <= "2018-06-30") %>%
        group_by(purchase_date) %>%
        summarise(salesbyday = sum(value))


# Plotting
t = gvisLineChart(t1, options = list(
        width = "auto", height = "800px"
))
plot(t)

# Generating analysis by categories
c1 = o3 %>%
        na.omit(product_category) %>%
        group_by(product_category) %>%
        summarise(total_sales = sum(value), 
                  salesperitem = round(sum(value)/n(),2),
                  avg_review = round(mean(review_score), 3)) %>%
        arrange(desc(total_sales))

# Plotting
c = gvisBarChart(c1[,1:2], options = list(
        width = "auto", height = "800px", bar = "{groupWidth: '80%'}",
        hAxis = "{title:'Sales', format: 'short', scaleType: 'log'}", 
        animation = "{startup: true}"
))
plot(c)


#Plotting attempt
m = leaflet(states) %>%
        addTiles()

m %>% addPolygons()

summary(revbystate$totalrev)
bins = c(0, 100000, 200000, 300000, 400000, 1000000, 2000000, 5000000, Inf)
pal = colorBin("YlGn", domain = s1$sales, bins = bins)

rbsplot = m %>% addPolygons(
        fillColor = ~pal(s1$sales),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

rbsplot %>% addLegend(pal = pal, values = s1$sales, opacity = 0.7, title = NULL, 
                      position = "bottomright")

labels <- sprintf(
        "<strong>%s</strong><br/><strong>Revenue:</strong> $%g BRL",
        states$nome, s1$sales
) %>% lapply(htmltools::HTML)


o3 = o2 %>%
        group_by(city) %>%
        summarise(n = n())

states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
City
plot(geo1)

