# app.R
# Main JoesFlowHPC app

# library(JoesFlowHPC)
#
# load_JoesFlow_config('Skyline_dev', 'inst/config.yml')

library(shiny)

ui <- fluidPage(
    textOutput("user_name")
)

server <- function(input, output, session) {
    output$user_name <- renderText({
        paste0("Hello ", Sys.getenv("USER"), "!")
        #paste("Logged in as:", user_info$username)
    })
}

shinyApp(ui = ui, server = server)
