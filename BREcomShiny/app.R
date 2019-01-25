
ui <- fluidPage(
        shinyUI(
                dashboardPage(
                        dashboardHeader(
                                title = "Brazilian E-commerce"
                        ),
                        dashboardSidebar(
                                sidebarUserPanel(
                                        "Charlie Wang"
                                ),
                                sidebarMenu(
                                        menuItem("Geographic", tabName = "geo", icon = icon("map")),
                                        menuItem("Trends", tabName = "time", icon = icon("calendar-alt")),
                                        menuItem("Categories", tabName = "cat", icon = icon("box")),
                                        menuItem("Data", tabName = "dat", icon = icon("database"))
                                )
                        ),
                        dashboardBody(
                                tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
                                tabItems(
                                        tabItem(tabName = "geo",
                                                fluidRow(
                                                        box(selectizeInput("geoin", "Select Item to Display", geo_choices),
                                                            leafletOutput("geo"),
                                                            width = "auto",
                                                            height = "100%")
                                                )
                                        ),
                                        tabItem(
                                                tabName = "time",
                                                fluidRow(
                                                        box(dateRangeInput("datein", "Select Range to Show Sales", 
                                                                           start = head(time_df$purchase_date,1), 
                                                                           end = tail(time_df$purchase_date, 1),
                                                                           min = head(time_df$purchase_date,1),
                                                                           max = tail(time_df$purchase_date, 1)),
                                                            htmlOutput("tim"),
                                                            width = "auto",
                                                            height = "100%")
                                                )
                                        ),
                                        tabItem(
                                                tabName = "cat",
                                                fluidRow(
                                                        box(pickerInput(inputId = "cats", 
                                                                        label = "Select Categories to Show", 
                                                                        choices = cats_choices, selected = cats_choices,
                                                                        options = list(`actions-box` = TRUE), 
                                                                        multiple = TRUE),
                                                            selectInput("catvalue", "Select Value to Show", 
                                                                        choices = catvalue_choices, selected = "total_sales"),
                                                            htmlOutput("cat"),
                                                            width = "auto",
                                                            height = "800")
                                                )
                                        ),
                                        tabItem(
                                                tabName = "dat",
                                                fluidRow(
                                                        box(DT::dataTableOutput("dat"),
                                                            width = "12")
                                                )
                                        )
                                )
                        )
                )
        )
)

server <- function(input, output, session) {
        bins = reactiveValues()
        labtxt = reactiveValues()

        
# Filtering date range input
        time_df1 = reactive({
                req(input$datein)
                
                time_df %>%
                        filter(purchase_date >= input$datein[1] & purchase_date <= input$datein[2])

        })
        
        cat_df1 = reactive({
                req(input$catvalue)
                cat_df %>%
                        filter(product_category %in% input$cats) %>%
                        select(product_category, input$catvalue)
                        
        })
        
        
        observe({
                if (input$geoin == "sales") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Sales:</strong> $%g BRL"
                        bins$y = c(0, 50000, 100000, 200000, 300000, 400000, 1000000, 2000000, 5000000, Inf)
                } else if (input$geoin == "avg_shcsratio") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Ratio:</strong> %g"
                        bins$y = 9
                } else if (input$geoin == "avg_review") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Score:</strong> %g"
                        bins$y = 9
                } else if (input$geoin %in% c("avg_delidays", "avg_diffestdel")) {
                        labtxt$x = "<strong>%s</strong><br/>%g Days"
                        bins$y = 9
                } else {
                        labtxt$x = "<strong>%s</strong><br/>$%g BRL" 
                        bins$y = 9
                }
                

        })

# Graph for Map
        output$geo = renderLeaflet({
                pal = colorBin("Greens",geo_df[,input$geoin], bins = bins$y, pretty = F)
                labels = sprintf(
                        labtxt$x,
                        states$nome, geo_df[,input$geoin]
                ) %>% lapply(htmltools::HTML)
                
                geo = leaflet(states) %>%
                        addTiles() %>%
                        addPolygons(
                                fillColor = ~pal(geo_df[,input$geoin]),
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
                geo %>% 
                        addLegend(pal = pal, values = geo_df[,input$geoin], opacity = 0.7, title = NULL, position = "bottomright")
                })


# Time analysis
        output$tim = renderGvis({
                gvisLineChart(time_df1(), options = list(
                        width = "automatic", height = "600px", legend = "none", vAxis = "{title: 'Sales (in $BRL)', format: 'short'}",
                        hAxis = "{title: 'Date', format: 'MMM d, y'}"
                ))
        })


# Categorical analysis
        output$cat = renderGvis(
                gvisBarChart(cat_df1(), options = list(
                        width = "automatic", height = "800", bar = "{groupWidth: '80%'}",
                        hAxis = "{title:'Sales', format: 'short', scaleType: 'log'}", 
                        animation = "{startup: true}", legend = "none"
                ))
        )


# Data table
        output$dat = DT::renderDataTable({
                datatable(order_df, rownames = F)
        })
}


shinyApp(ui = ui, server = server)

