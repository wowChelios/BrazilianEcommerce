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
                                                                                width = 1400,
                                                                                height = 800
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
                                                                                title = "Select to Display on Map",
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
                                                                                title = "Trend Line Chart Output",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                height = 1200,
                                                                                status = "info",
                                                                                htmlOutput("tim")
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Select Range to Show Sales",
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
                                                                                title = "Select Categories to Show",
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
                                                                                title = "Data",
                                                                                solidHeader = T,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                status = "info",
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
                                                                                title = "Bar Chart Output",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                height = 1200,
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
        
        # Reactive Table for Categories Trend table
        cat_time_table = reactive({
                req(input$datein)
                req(input$trdcats)

                
                cat_time_df %>%
                        filter(purchase_date >= input$datein[1] &
                                       purchase_date <= input$datein[2]) %>%
                        select(Date = purchase_date, input$trdcats)
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
                        labtxt$x = "<strong>%s</strong><br/><strong>Sales:</strong> $%g BRL"
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
                pal = colorBin("Greens",
                               geo_df[, input$geoin],
                               bins = bins$y,
                               pretty = F)
                labels = sprintf(labtxt$x,
                                 states$nome, geo_df[, input$geoin]) %>% lapply(htmltools::HTML)
                
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
                gvisScatterChart(geo_df_scat(),
                                 options = list(
                                         width = "300px",
                                         height = "300px",
                                         legend = "none"
                                 ))
        })
        
        # Printing correlation
        output$cor = renderText({
                paste("Correlation:", round(cor(geo_corx(), geo_cory())[[1]], 2), sep = " ")
        })
        
        # Geo Data Output
        output$table = renderTable({
                head(geo_df_table(), 10)
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
                                height = "1000px",
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
        
        ?renderDataTable
        
        
        # Categories Bar Chart
        output$cat = renderGvis(gvisBarChart(
                cat_df_bar(),
                options = list(
                        width = "automatic",
                        height = "1000px",
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
