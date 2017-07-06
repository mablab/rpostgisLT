
keyboardCounter <- function(pgtraj_sf,
                         d_start,
                         t_start,
                         tzone,
                         increment,
                         interval) {
    
    ui <- bootstrapPage(
        mainPanel(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        h1(textOutput("counter")),
        tags$script(
            '$(document).on("keydown",
            function (e) {
                if(e.which == 66) {
                    Shiny.onInputChange("b", new Date());
                } else if (e.which == 78) {
                    Shiny.onInputChange("n", new Date());
                }
            });
            '
    ),
    actionButton("b", "Back"),
    actionButton("n", "Next"),
    h5("press B or N")
        )
    )
    
    server <- function(input, output) {

        x <- reactiveValues(counter = 0)
        
        # Only update timestamp on click
        observeEvent(input$n, {
            x$counter <- x$counter + 1
        })
        
        observeEvent(input$b, {
            x$counter <- x$counter - 1
        })
        
        # Report current timestamp
        output$counter <- renderText({
            x$counter
        })
        
    }
    shinyApp(ui, server)
    
    }

keyboardCounter()