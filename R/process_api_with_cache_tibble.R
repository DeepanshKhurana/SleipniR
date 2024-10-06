#' Process an API request with cache and return a tibble
#' @param endpoint The endpoint to process.
#' @param force_refresh Whether to force a refresh of the cache.
#' @return A list with tibble with the response and the status of the cache.
#' @export
process_api_with_cache_tibble <- function(
  endpoint,
  force_refresh = FALSE
) {
  response <- process_api_with_cache(
    endpoint = endpoint,
    force_refresh = force_refresh
  )
  list(
    "response" = dplyr::bind_rows(
      lapply(
        response$response,
        unlist
      )
    )  |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ sapply(
            .x,
            cast_type
          )
        )
      ),
    "status" = response$status
  )
}
