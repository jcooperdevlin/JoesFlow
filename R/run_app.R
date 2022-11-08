#' Run a local instance of JoesFlow
#'
#' This function will start up a local instance of JoesFlow from within RStudio.
#'
#' @details While this is useful, the system requirements can take some time to set up properly. We recommend the use of Docker (see https://github.com/IDSS-NIAID/JoesFlow for more documentation on using Docker).
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function()
{
  shinyApp(ui = app_ui(),
           server = app_server)
}
