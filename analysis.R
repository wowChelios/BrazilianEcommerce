getwd()
setwd('..')
setwd('Desktop/Qifanworkspace/R/BrazilianEcommerce')
library(tidyverse)
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

#Category analysis
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

#Cleaning Geographical data
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

#Cleaning Payment Table
p1 = payments %>%
        group_by(order_id) %>%
        summarise(value = sum(payment_value))

#Joining all tables
o1 = orders %>%
        left_join(p1, by = "order_id") %>%
        left_join(customer, by = "customer_id") %>%
        left_join(g1, by = c("customer_zip_code_prefix" = "geolocation_zip_code_prefix")) %>%
        left_join(reviews, by = "order_id") %>%
        left_join(items, by = "order_id") %>%
        left_join(productnames, by = "product_id") %>%
        select(order_id, customer_id, order_status,order_purchase_timestamp, order_delivered_customer_date, 
               value, customer_unique_id, customer_zip_code_prefix, lat, lng, customer_city, customer_state, 
               review_score, product_id, product_category_name_english)

#Cleaning Gaint Table Data
o2 = o1 %>%
        filter(order_status != "canceled") %>%
        rename(purchase_date = order_purchase_timestamp, delivered_date = order_delivered_customer_date, 
               zip = customer_zip_code_prefix, city = customer_city, state = customer_state, 
               product_category = product_category_name_english) %>%
        mutate(purchase_date = as_datetime(purchase_date), delivered_date = as_datetime(delivered_date))

class(o2$purchase_date)
?left_join
o1 = orders %>%
        group_by()