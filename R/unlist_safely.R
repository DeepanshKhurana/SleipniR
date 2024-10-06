#' Unlist a list safely
#' @param x The list to unlist.
#' @return The list unlisted.
unlist_safely <- function(x) {
  sapply(
    unlist(x),
    cast_type
  )
}
