#' load_JoesFlow_config
#' Load the configuration file for JoesFlow
#'
#' @param profile The profile to load
#' @param config_path Path to the configuration file (defaults to the configuration file installed with the package)
#'
#' @details This will load the configuration file for JoesFlowHPC into a variable in the global environment called `.JoesFlow`.
#'
#' @return Invisibly returns the configuration list
#' @export
load_JoesFlow_config <- function(profile = 'default', config_path = NULL) {

  if (is.null(config_path)) {
    config_path <- system.file('config.yml',
                               package = 'JoesFlowHPC',
                               mustWork = TRUE)
  }

  # load the configuration file
  config <- config::get(file = config_path,
                        config = profile)

  # pull things from setup script (should add these to `config`)
  if(!is.null(config$setup_script))
  {
    source(config$setup_script)
  }

  # load the profile into the global environment
  assign('.JoesFlow', config, envir = .GlobalEnv)

  invisible(config)
}


#' setup_userDB
#' Setup the user database
#'
#' @param db_path Path to the database

### package configuration

# load the configuration file
config <- config::get(file = 'inst/config.yml')

# eventually this will be loaded with
# config <- config::get(file = system.file('config.yml',
#                                          package = 'JoesFlowHPC',
#                                          mustWork = TRUE))


### Sending commands to Skyline

library(ssh)

con <- ssh_connect(host = "johnsonra@ai-hpcsubmit1.niaid.nih.gov",
                   keyfile = "~/.ssh/id_rsa",
                   passwd = system("op read op://Private/JoesFlow/Skyline_key", intern = TRUE))

ssh_exec_wait(con, "ls -l ~/rtb_idss/")

ssh_disconnect(con)

### getting current user

library(R.utils)

System$getUsername()

### Notes

# app runs at
# dev: file.path(here::here(), "Shiny")
# test: /home/johnsonra/JoesFlow/shiny
# prod: /opt/rstudio-connect/mnt/app

# ls (it has whaterver we push with the app)
# prod: app.R, manifest.json, packrat

### setting up the user database - use an environmental variable - on the Posit connect server, this will need to be manually set under the Vars pannel of the admin options :)

library(shinymanager)
