# Skyline_dev environmental variables

root <- here::here()

Sys.setenv(jf_user = Sys.getenv("USER"))
Sys.setenv(jf_db_path = file.path(root, "Shiny", "jf_users.sqlite"))
Sys.setenv(jf_db_pass = system("op read op://Private/JoesFlow/jf_db_pass", intern = TRUE))

Sys.setenv(hpc_rsa_pass = system("op read op://Private/JoesFlow/Skyline_key", intern = TRUE))
