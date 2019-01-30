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

ui <- fluidPage(theme = "style.css",
                shinyUI(
                        dashboardPage(
                                skin = "blue",
                                dashboardHeader(title = "Brazilian E-commerce"),
                                dashboardSidebar(sidebarMenu(
                                        menuItem(
                                                "Introduction",
                                                tabName = "intr",
                                                icon = icon("align-justify")
                                        ),
                                        menuItem("Geographic", tabName = "geo", icon = icon("map")),
                                        menuItem("Trends", tabName = "time", icon = icon("line-chart")),
                                        menuItem(
                                                "Categories",
                                                tabName = "cat",
                                                icon = icon("dashboard")
                                        )
                                )),
                                dashboardBody(
                                        tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
                                        tabItems(
                                                tabItem(tabName = "intr",
                                                        fluidRow(column(
                                                                width = 12,
                                                                box(
                                                                        title = "Introduction",
                                                                        solidHeader = T,
                                                                        width = NULL,
                                                                        status = "info",
                                                                        id = "intro",
                                                                        tags$h1("About This Project"),
                                                                        tags$h3(
                                                                                "Half the money I spend on advertising is wasted; the trouble is, I dont't know which half' -- John Wanamaker(1838-1922)"
                                                                        ),
                                                                        tags$h4(intro_str),
                                                                        tags$h2("The Dataset"),
                                                                        tags$h4(intrdata_str),
                                                                        tags$img(
                                                                                src = "HRhd2Y0.png",
                                                                                width = 1100,
                                                                                height = 660
                                                                        )
                                                                )
                                                                
                                                        ))),
                                                tabItem(tabName = "geo",
                                                        fluidRow(
                                                                column(
                                                                        width = 9,
                                                                        box(
                                                                                title = "Map",
                                                                                solidHeader = T,
                                                                                status = "info",
                                                                                leafletOutput("geo", height = 800),
                                                                                width = NULL,
                                                                                height = "auto"
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Select to Plot",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                selectizeInput("geoin", label = NULL, geo_choices)
                                                                        ),
                                                                        box(
                                                                                id = "scatter",
                                                                                title = "Scatter Plot",
                                                                                solidHeader = T,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                dropdownButton(
                                                                                        tags$h3("List of Input"),
                                                                                        selectInput(
                                                                                                inputId = 'xcol',
                                                                                                label = 'X Variable',
                                                                                                choices = xcol_choices,
                                                                                                selected = xcol_choices[[1]]
                                                                                        ),
                                                                                        selectInput(
                                                                                                inputId = 'ycol',
                                                                                                label = 'Y Variable',
                                                                                                choices = ycol_choices,
                                                                                                selected = ycol_choices[[1]]
                                                                                        ),
                                                                                        circle = TRUE,
                                                                                        status = "info",
                                                                                        icon = icon("gear"),
                                                                                        width = "250px",
                                                                                        tooltip = tooltipOptions(title = "Correlations")
                                                                                ),
                                                                                htmlOutput("geoscat"),
                                                                                tags$h4(textOutput("cor"))
                                                                        ),
                                                                        box(
                                                                                title = "Data",
                                                                                solidHeader = T,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                tableOutput({
                                                                                        "table"
                                                                                })
                                                                        )
                                                                )
                                                        )),
                                                
                                                tabItem(tabName = "time",
                                                        fluidRow(
                                                                column(
                                                                        width = 9,
                                                                        box(
                                                                                title = "Trends",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                height = 1000,
                                                                                status = "info",
                                                                                htmlOutput("tim")
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Date Range Input",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                dateRangeInput(
                                                                                        "datein",
                                                                                        label = NULL,
                                                                                        start = head(time_df$purchase_date, 1),
                                                                                        end = tail(time_df$purchase_date, 1),
                                                                                        min = head(time_df$purchase_date, 1),
                                                                                        max = tail(time_df$purchase_date, 1)
                                                                                )
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Categories To Plot",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                pickerInput(
                                                                                        inputId = "trdcats",
                                                                                        choices = trd_choices,
                                                                                        selected = trd_choices[12],
                                                                                        options = list(`actions-box` = TRUE),
                                                                                        multiple = TRUE
                                                                                )
                                                                        ),
                                                                        box(
                                                                                title = "Category Input",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "warning",
                                                                                selectInput("catsfortable", label = NULL, trd_choices)
                                                                        ),
                                                                        box(
                                                                                title = "Data",
                                                                                solidHeader = T,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                status = "warning",
                                                                                DT::dataTableOutput({
                                                                                        "trdtable"
                                                                                })
                                                                        )
                                                                )
                                                        )),
                                                tabItem(tabName = "cat",
                                                        fluidRow(
                                                                column(
                                                                        width = 9,
                                                                        box(
                                                                                title = "Bar Chart",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                height = 1000,
                                                                                status = "info",
                                                                                htmlOutput("cat")
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Variables Input",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                selectInput(
                                                                                        "catvalue",
                                                                                        label = NULL,
                                                                                        choices = catvalue_choices,
                                                                                        selected = "total_sales"
                                                                                )
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Categories Input",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                pickerInput(
                                                                                        inputId = "cats",
                                                                                        label = NULL,
                                                                                        choices = cats_choices,
                                                                                        selected = cats_choices,
                                                                                        options = list(`actions-box` = TRUE),
                                                                                        multiple = TRUE
                                                                                )
                                                                        ),
                                                                        box(
                                                                                title = "Data",
                                                                                solidHeader = T,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                tableOutput({
                                                                                        "cattable"
                                                                                })
                                                                        )
                                                                )
                                                        ))
                                        )
                                )
                        )
                ))

server <- function(input, output, session) {
        bins = reactiveValues()
        labtxt = reactiveValues()
        
        
        # Reactive Data For scatter Plot
        geo_df_scat = reactive({
                req(input$xcol, input$ycol)
                
                geo_df %>%
                        select(input$xcol, input$ycol)
                
        })
        
        # Reactive Data For Correlation computing
        geo_corx = reactive({
                req(input$xcol)
                
                geo_df %>%
                        select(input$xcol)
        })
        geo_cory = reactive({
                req(input$ycol)
                
                geo_df %>%
                        select(input$ycol)
        })
        
        # Reactive Data For geo Table
        geo_df_table = reactive({
                req(input$geoin)
                geo_df %>%
                        select(state, value =  input$geoin) %>%
                        arrange(desc(value))
        })
        
        # Reactive Data For Line chart
        cat_time_df_line = reactive({
                req(input$datein)
                req(input$trdcats)
                
                cat_time_df %>%
                        select(purchase_date, input$trdcats) %>%
                        filter(purchase_date >= input$datein[1] &
                                       purchase_date <= input$datein[2])
                
        })
        
        # Reactive Data for Categories Trend table
        cat_time_table = reactive({
                req(input$datein)
                req(input$catsfortable)
                
                
                cat_time_df %>%
                        filter(purchase_date >= input$datein[1] &
                                       purchase_date <= input$datein[2]) %>%
                        select(Date = purchase_date, input$catsfortable)
        })
        
        # Reactive Data For Bar Chart and Table
        cat_df_bar = reactive({
                req(input$catvalue)
                cat_df %>%
                        filter(category %in% input$cats) %>%
                        select(category, value = input$catvalue) %>%
                        arrange(., desc(value))
                
        })
        
        # Switching labels for map
        observe({
                if (input$geoin == "sales") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Sales:</strong> $%s BRL"
                        bins$y = c(
                                0,
                                50000,
                                100000,
                                200000,
                                300000,
                                400000,
                                1000000,
                                2000000,
                                5000000,
                                Inf
                        )
                } else if (input$geoin == "avg_shcsratio") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Ratio:</strong> %s"
                        bins$y = 9
                } else if (input$geoin == "avg_review") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Score:</strong> %s"
                        bins$y = 9
                } else if (input$geoin %in% c("avg_delidays", "avg_diffestdel")) {
                        labtxt$x = "<strong>%s</strong><br/>%s Days"
                        bins$y = 9
                } else {
                        labtxt$x = "<strong>%s</strong><br/>$%s BRL"
                        bins$y = 9
                }
                
                c = input$trdcats
                
                if (is.null(c))
                        c = character(0)
                
                updateSelectInput(
                        session,
                        "catsfortable",
                        choices = c,
                        selected = head(c, 1)
                )
                
        })
        
        # Graph for Map
        output$geo = renderLeaflet({
                pal = colorBin("Greens",
                               geo_df[, input$geoin],
                               bins = bins$y,
                               pretty = F)
                labels = sprintf(labtxt$x,
                                 states$nome,
                                 format(
                                         geo_df[, input$geoin],
                                         scientific = F,
                                         big.mark = ","
                                 )) %>% lapply(htmltools::HTML)
                
                geo = leaflet(states) %>%
                        addTiles() %>%
                        addPolygons(
                                fillColor = ~ pal(geo_df[, input$geoin]),
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
                                        bringToFront = TRUE
                                ),
                                label = labels,
                                labelOptions = labelOptions(
                                        style = list(
                                                "font-weight" = "normal",
                                                padding = "3px 8px"
                                        ),
                                        textsize = "15px",
                                        direction = "auto"
                                )
                        )
                geo %>%
                        addLegend(
                                pal = pal,
                                values = geo_df[, input$geoin],
                                opacity = 0.7,
                                title = NULL,
                                position = "bottomright"
                        )
        })
        
        #Geo scatter plot
        output$geoscat = renderGvis({
                gvisScatterChart(
                        geo_df_scat(),
                        options = list(
                                width = "300px",
                                height = "300px",
                                legend = "none"
                        )
                )
        })
        
        # Printing correlation
        output$cor = renderText({
                paste("Correlation:", round(cor(geo_corx(), geo_cory())[[1]], 2), sep = " ")
        })
        
        # Geo Data Output
        output$table = renderTable({
                head(geo_df_table(), 6)
        },
        striped = T,
        spacing = 'l',
        width = '100%',
        colnames = F,
        digits = 2)
        
        
        # Trend Line Chart
        output$tim = renderGvis({
                gvisLineChart(
                        cat_time_df_line(),
                        options = list(
                                width = "automatic",
                                height = "800px",
                                vAxis = "{title: 'Sales (in $BRL)', format: 'short'}",
                                hAxis = "{title: 'Date', format: 'MMM d, y'}",
                                animation = "{startup: true}"
                        )
                )
        })
        
        # Trend table
        output$trdtable = DT::renderDataTable({
                datatable(cat_time_table(), rownames = F)
        })
        
        
        
        # Categories Bar Chart
        output$cat = renderGvis(gvisBarChart(
                cat_df_bar(),
                options = list(
                        width = "automatic",
                        height = "800px",
                        bar = "{groupWidth: '80%'}",
                        hAxis = "{title:'Sales (in $BRL)', format: 'short', scaleType: 'log'}",
                        animation = "{startup: true}",
                        legend = "none"
                )
        ))
        
        # Categories Table
        output$cattable = renderTable({
                head(cat_df_bar(), 10)
        },
        striped = T,
        spacing = 'l',
        width = '100%',
        colnames = F)
        
}

shinyApp(ui = ui, server = server)
