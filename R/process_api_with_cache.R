#' Process an API request with cache.
#' @param endpoint The endpoint to process.
#' @return The response from the API.
#' @export
process_api_with_cache <- function(endpoint) {
  cached_status <- get_cache_status(endpoint)
  if (cached_status$cron_status %in% c("yellow", "green")) {
    process_api(endpoint, cached = TRUE)
  } else {
    process_api(endpoint, cached = FALSE)
  }
}
