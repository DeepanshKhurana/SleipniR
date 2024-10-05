#' Get the API key from the environment
#' @return The API key
get_api_key <- function(
) {
  api_key <- Sys.getenv("API_KEY")
  if (nchar(api_key) > 1) {
    api_key
  } else {
    stop("API_KEY not set as an environment variable.")
  }
}
