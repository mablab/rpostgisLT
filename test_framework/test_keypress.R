library(shiny)


shinyApp(ui <- pageWithSidebar(
    headerPanel("Test keyboard control"),
    sidebarPanel(
        tags$script(
            'tags$head(
            $(document).keydown(function(e)){
                if (e.keyCode == 85) {
                    Shiny.onInputChange("upButton", new Date());
                } else if (e.keyCode == 68) {
                    Shiny.onInputChange("downButton", new Date());
                }
            });'
        ),
        actionButton("downButton", "Down"),
        actionButton("upButton", "Up")
    ),
    mainPanel(htmlOutput("text"))
),

server <- function(session, input, output) {
    vals <- reactiveValues(count = 0)
    
    observeEvent(input$downButton, {vals$count <- vals$count - 1})
    observeEvent(input$upButton, {vals$count <- vals$count + 1})
    
    output$text <- renderText(paste("Counter is:", vals$count))
}
)


