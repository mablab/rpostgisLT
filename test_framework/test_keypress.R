library(shiny)
shinyApp(ui <- pageWithSidebar(
    headerPanel("Test keyboard control"),
    sidebarPanel(
        tags$script('$(document).on("keydown",
                 function (e) {
                 if(e.which == 68) {
                   Shiny.onInputChange("downButton", new Date());
                 } else if (e.which == 85) {
                   Shiny.onInputChange("upButton", new Date());
                 }
                 });
                '),
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
