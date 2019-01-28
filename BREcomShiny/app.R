
ui <- fluidPage(theme = "style.css",
        shinyUI(
                dashboardPage(
                        skin = "green",
                        dashboardHeader(
                                title = "Brazilian E-commerce"
                        ),
                        dashboardSidebar(
                                sidebarUserPanel(
                                        "Charlie Wang"
                                ),
                                sidebarMenu(
                                        menuItem("Introduction", tabName = "intr", icon = icon("align-justify")),
                                        menuItem("Geographic", tabName = "geo", icon = icon("map")),
                                        menuItem("Trends", tabName = "time", icon = icon("line-chart")),
                                        menuItem("Categories", tabName = "cat", icon = icon("dashboard"))
                                )
                        ),
                        dashboardBody(
                                tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
                                tabItems(
                                        tabItem(tabName = "intr",
                                                fluidRow(tags$div(class = "boxes",
                                                        column(
                                                                width = 12,tags$div(class = "intro",
                                                                box(width = NULL, status = "success",
                                                                        tags$h1("About This Project"),
                                                                        tags$h3("Half the money I spend on advertising is wasted; the trouble is, I dont't know which half' -- John Wanamaker(1838-1922)"),
                                                                        tags$h4(intro_str),
                                                                        tags$h2("The Dataset"),
                                                                        tags$h4(intrdata_str),
                                                                        tags$img(src = "HRhd2Y0.png", width = 1200, height = 800))
                                                                )
                                                        ))
                                                )
                                        ),
                                        tabItem(tabName = "geo",
                                                fluidRow(column(width = 9, tags$div(class = "output",
                                                        box(status = "success",
                                                            leafletOutput("geo", height = 800),
                                                            width = NULL,
                                                            height = "auto"))),
                                                        column(width = 3,tags$div(class = "input",
                                                               box(width = NULL, status = "warning",
                                                                   selectizeInput("geoin", "Select to Display on Map", geo_choices))), tags$div(class = "input",
                                                               box(width = NULL, status = "warning",tags$h3("Correlation Between Variables"),
                                                                   dropdownButton(
                                                                           tags$h3("List of Input"),
                                                                           selectInput(inputId = 'xcol', label = 'X Variable', choices = xcol_choices, selected = xcol_choices[[1]]),
                                                                           selectInput(inputId = 'ycol', label = 'Y Variable', choices = ycol_choices, selected = ycol_choices[[1]]),
                                                                           circle = TRUE, status = "success", icon = icon("gear"), width = "250px",
                                                                           tooltip = tooltipOptions(title = "Correlations")
                                                                   ), htmlOutput("geoscat"))))
                                                )
                                        ),
              
                                        tabItem(
                                                tabName = "time",
                                                fluidRow(column(width = 6, tags$div(class = "input", box(width = NULL, status = "warning",pickerInput(inputId = "trdcats", 
                                                                                                               label = "Select Categories to Show", 
                                                                                                               choices = trd_choices, selected = trd_choices[12],
                                                                                                               options = list(`actions-box` = TRUE), 
                                                                                                               multiple = TRUE)))),
                                                         column(width = 6, tags$div(class = "input", box(width = NULL, status = "warning", dateRangeInput("datein", "Select Range to Show Sales", 
                                                                                                                  start = head(time_df$purchase_date,1), 
                                                                                                                  end = tail(time_df$purchase_date, 1),
                                                                                                                  min = head(time_df$purchase_date,1),
                                                                                                                  max = tail(time_df$purchase_date, 1))))), 
                                                         column(width = 12, tags$div(class = "output",box(width = NULL, status = "success", htmlOutput("tim"))))
                                                        )
                                        ),
                                        tabItem(
                                                tabName = "cat",
                                                fluidRow(column(width = 6, tags$div(class = "input", box(width = NULL, status = "warning", pickerInput(inputId = "cats", 
                                                                                                                             label = "Select Categories to Show", 
                                                                                                                             choices = cats_choices, selected = cats_choices,
                                                                                                                             options = list(`actions-box` = TRUE), 
                                                                                                                             multiple = TRUE) ))),
                                                         column(width = 6, tags$div(class = "input", box(width = NULL, status = "warning", selectInput("catvalue", "Select Value to Show", 
                                                                                                                             choices = catvalue_choices, selected = "total_sales")))),
                                                         column(width = 12, tags$div(class = "output",box(width = NULL, status = "success", htmlOutput("cat"))))
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

        
# Filtering inputs
        geo_df1 = reactive({
                req(input$xcol, input$ycol)
                
                geo_df %>%
                        select(input$xcol, input$ycol)
        })
        
        cat_time_df1 = reactive({
                req(input$datein)
                req(input$trdcats)
                
                cat_time_df %>%
                        select(purchase_date, input$trdcats) %>%
                        filter(purchase_date >= input$datein[1] & purchase_date <= input$datein[2])

        })
        
        cat_df1 = reactive({
                req(input$catvalue)
                cat_df %>%
                        filter(category %in% input$cats) %>%
                        select(category, input$catvalue) %>%
                        arrange(desc(!!input$catvalue))
                        
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
        
#Geo scatter plot 
        output$geoscat = renderGvis({
                gvisScatterChart(geo_df1(), options = list(
                        width = "500px", height = "500px", legend = "none"
                ))
        })


# Time analysis
        output$tim = renderGvis({
                gvisLineChart(cat_time_df1(), options = list(
                        width = "automatic", height = "800px", vAxis = "{title: 'Sales (in $BRL)', format: 'short'}",
                        hAxis = "{title: 'Date', format: 'MMM d, y'}"
                ))
        })


# Categorical analysis
        output$cat = renderGvis(
                gvisBarChart(cat_df1(), options = list(
                        width = "automatic", height = "800px", bar = "{groupWidth: '80%'}",
                        hAxis = "{title:'Sales (in $BRL)', format: 'short', scaleType: 'log'}", 
                        animation = "{startup: true}", legend = "none"
                ))
        )

}

shinyApp(ui = ui, server = server)

