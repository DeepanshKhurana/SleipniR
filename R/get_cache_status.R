#' Get the cache status of an endpoint.
#' @param endpoint The endpoint to check the cache status of.
#' @return A list with the cache status.
get_cache_status <- function(
  endpoint = NULL
) {
  checkmate::assert_string(endpoint)
  response <- httr2::request(make_endpoint("staleness")) |>
    httr2::req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    httr2::req_url_query(
      endpoint = endpoint
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  lapply(
    response,
    unlist
  )
}
