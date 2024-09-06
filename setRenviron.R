# Skyline_prod environmental variables

library(httr)
library(jsonlite)

# Define the API endpoint
api_url <- paste0(Sys.getenv("CONNECT_SERVER"), "/__api__/v1/user")

# Get the API key from environment variable or configuration
api_key <- Sys.getenv("CONNECT_API_KEY")

# Make the API request
response <- GET(api_url, add_headers(Authorization = paste("Key", api_key)))

# Check if the request was successful
if (status_code(response) == 200) {
  Sys.setenv(jf_user = fromJSON(content(response, "text", encoding = "UTF-8"))$username)
} else {
  Sys.setenv(jf_user = 'unknown')
}
