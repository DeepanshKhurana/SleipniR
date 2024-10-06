#' Process an API request with cache.
#' @param endpoint The endpoint to process.
#' @param force_refresh Whether to force a refresh of the cache.
#' @return A list with the response and the status of the cache.
#' @export
process_api_with_cache <- function(
  endpoint,
  force_refresh = FALSE
) {
  cached_status <- get_cache_status(endpoint)$cron_status
  if (force_refresh) {
    cached_status <- "green"
    response <- process_api(endpoint, cached = FALSE)
  } else if (cached_status %in% c("red", "yellow", "green")) {
    response <- process_api(endpoint, cached = TRUE)
  } else {
    stop("Invalid cache status in process_api_with_cache")
  }
  list(
    response = response,
    status = cached_status
  )
}
