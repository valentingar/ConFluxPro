#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    fluidRow(
        column(9,
            numericInput("n", "Number of sections", value = 5, min = 1)
        )),
    fluidRow(
        column(3,
               uiOutput("sects_start")
               ),
        column(3,
               uiOutput("sects_end")),
        column(3,
               uiOutput("sects_mode"))
    ),
    fluidRow(
        column(9,
        tableOutput("corr_map"),
        plotOutput("distPlot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    sects_start <- reactive(paste0("sects_start", seq_len(input$n)))
    sects_end <- reactive(paste0("sects_end", seq_len(input$n)))
    sects_mode <- reactive(paste0("sects_mode", seq_len(input$n)))

    output$sects_start <- renderUI({
        map(sects_start(), ~ dateInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
    })
    output$sects_end <- renderUI({
        map(sects_end(), ~ dateInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
    })
    output$sects_mode <- renderUI({
        map(sects_mode(), ~ textInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
    })


    output$corr_map <- renderTable({
        corr_map <-data_frame(section = purrr::map_chr(sects_start(),default_val(input[[.x]], NA)))#,
                            #start = input$sects_start,
                            #end = input$sects_end

    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        df %>% left_join(corr_map) %>%

        ggplot2::ggplot(aes(x=Date,y=NRESULT_ppm,col=as.factor(section)))+geom_point()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
