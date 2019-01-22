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
