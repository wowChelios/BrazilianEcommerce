
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
                                        menuItem("Geographical Analysis", tabName = "geo", icon = icon("map")),
                                        menuItem("Time Analysis", tabName = "time", icon = icon("calendar-alt")),
                                        menuItem("Categorical Analysis", tabName = "cat", icon = icon("box")),
                                        menuItem("Data", tabName = "dat", icon = icon("database"))
                                )
                        ),
                        dashboardBody(
                                tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
                                tabItems(
                                        tabItem(tabName = "geo",
                                                fluidRow(
                                                        box(selectizeInput("geoin", "Select Item to Display", geochoices),
                                                            leafletOutput("geo"),
                                                            width = "auto",
                                                            height = "100%")
                                                )
                                        ),
                                        tabItem(
                                                tabName = "time",
                                                fluidRow(
                                                        box(htmlOutput("tim"),
                                                            width = "auto",
                                                            height = "800")
                                                )
                                        ),
                                        tabItem(
                                                tabName = "cat",
                                                fluidRow(
                                                        box(htmlOutput("cat"),
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
        
        observe({
                if (input$geoin != "sales") {
                        bins$y = 9
                } else {
                        bins$y = c(0, 100000, 200000, 300000, 400000, 1000000, 2000000, 5000000, Inf)
                }
        })

# Graph for Map
        output$geo = renderLeaflet({
                pal = colorBin("Greens",geodf[,input$geoin], bins = bins$y, pretty = F)
                labels = sprintf(
                        "<strong>%s</strong><br/><strong>Revenue:</strong> $%g BRL",
                        states$nome, geodf[,input$geoin]
                ) %>% lapply(htmltools::HTML)
                
                geo = leaflet(states) %>%
                        addTiles() %>%
                        addPolygons(
                                fillColor = ~pal(geodf[,input$geoin]),
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
                        addLegend(pal = pal, values = geodf[,input$geoin], opacity = 0.7, title = NULL, position = "bottomright")
                })


# Time analysis
        output$tim = renderGvis({
                gvisLineChart(timdf, options = list(
                        width = "auto", height = "800px"
                ))
        })


# Categorical analysis
        output$cat = renderGvis(
                gvisBarChart(catdf[,1:2], options = list(
                        width = "auto", height = "800px", bar = "{groupWidth: '80%'}",
                        hAxis = "{title:'Sales', format: 'short', scaleType: 'log'}", 
                        animation = "{startup: true}"
                ))
        )


# Data table
        output$dat = DT::renderDataTable({
                datatable(maindf, rownames = F)
        })
}


shinyApp(ui = ui, server = server)

