shiny::fluidPage(
    shiny::titlePanel(paste("pgtraj name:", pgtraj)),
    
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::tags$script(
                '$(document).on("keydown",
                function (e) {
                if(e.which == 66) {
                Shiny.onInputChange("b", new Date() );
                } else if (e.which == 78) {
                Shiny.onInputChange("n", new Date() );
                }
                });
                '
        ),
        shinyWidgets::switchInput(
            inputId = "step_mode",
            label = "Step mode",
            value = FALSE
        ),
        shinyWidgets::radioGroupButtons(inputId = "color_choice", 
                                        label = "Color",
                                        choices = c("Animals", "Bursts"),
                                        selected = "Animals"
        ),
        shiny::selectizeInput(
            inputId = "burst_picker",
            label = "Bursts",
            choices = bursts_df$burst_name,
            multiple = TRUE
        ),
        shiny::fluidRow(
            shiny::column(6,
                          shiny::numericInput("increment", "Increment",
                                              value = increment@.Data,
                                              width = "100%"),
                          shiny::numericInput("interval", "Interval",
                                              value = interval@.Data,
                                              width = "100%")
            ),
            shiny::column(6,
                          shiny::selectInput(
                              "increment_unit",
                              label = "units",
                              choices = c(
                                  "years" = "years",
                                  "months" = "months",
                                  "days" = "days",
                                  "hours" = "hours",
                                  "minutes" = "minutes",
                                  "seconds" = "seconds"
                              ),
                              selected = unit_init,
                              width = "100%"
                          ),
                          shiny::selectInput(
                              "interval_unit",
                              label = "units",
                              choices = c(
                                  "years" = "years",
                                  "months" = "months",
                                  "days" = "days",
                                  "hours" = "hours",
                                  "minutes" = "minutes",
                                  "seconds" = "seconds"
                              ),
                              selected = unit_init,
                              width = "100%"
                          )
            )
        ),
        shiny::sliderInput(
            "range",
            "Time window:",
            min = time_params$tstamp_start,
            max = time_params$tstamp_last,
            value = c(time_params$tstamp_start, time_params$tstamp_start + interval),
            step = increment,
            timezone = tzone
        ),
        shiny::actionButton("b", "Back"),
        shiny::actionButton("n", "Next"),
        shiny::h5("press B or N")
        ),
        
        shiny::mainPanel(leaflet::leafletOutput("map"))
))