# Skyline_dev dynamic configuration

config$root <- here::here()

config$jf_user <- Sys.getenv("USER")
config$jf_db_path <- file.path(config$root, "Shiny", "jf_users.sqlite")
config$jf_db_pass <- system("op read op://Private/JoesFlow/jf_db_pass", intern = TRUE)

config$hpc_rsa_pass = system("op read op://Private/JoesFlow/Skyline_key", intern = TRUE)
