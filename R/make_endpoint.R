#' Make an endpoint.
#' @param ... The parts of the endpoint.
#' @return The endpoint.
make_endpoint <- function(
  ...
) {
  endpoint_path <- paste(c(...), collapse = "/")
  api_path <- Sys.getenv("API_PATH")
  if (nchar(api_path) > 1) {
    glue::glue("{api_path}{endpoint_path}")
  } else {
    stop("API_PATH not set as an environment variable.")
  }
}
