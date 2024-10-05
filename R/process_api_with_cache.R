#' Process an API request with cache.
#' @param endpoint The endpoint to process.
#' @return A list with the response and the status of the cache.
#' @export
process_api_with_cache <- function(endpoint) {
  cached_status <- get_cache_status(endpoint)
  if (cached_status$cron_status %in% c("yellow", "green")) {
    response <- process_api(endpoint, cached = TRUE)
  } else {
    response <- process_api(endpoint, cached = FALSE)
  }
  list(
    response = response,
    status = cached_status$cron_status
  )
}
