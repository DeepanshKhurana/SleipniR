#' Process an API request.
#' @param endpoint The endpoint to process.
#' @param cached Whether to use the cache or not.
#' @return The response from the API.
process_api <- function(
  endpoint = NULL,
  cached = FALSE
) {
  checkmate::assert_logical(cached)
  httr2::request(make_endpoint(endpoint)) |>
    httr2::req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    httr2::req_url_query(
      cached = cached
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
