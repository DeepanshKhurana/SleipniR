#' Process an API request with cache.
#' @param endpoint The endpoint to process.
#' @return A list with the response and the status of the cache.
#' @export
process_api_with_cache <- function(
  endpoint,
  force_refresh = FALSE
) {
  cached_status <- get_cache_status(endpoint)$cron_status
  if (cached_status %in% c("yellow", "green")) {
    response <- process_api(endpoint, cached = TRUE)
  } else if (cached_status %in% "red" || force_refresh) {
    cached_status <- "green"
    response <- process_api(endpoint, cached = FALSE)
  }
  list(
    response = response,
    status = cached_status
  )
}
