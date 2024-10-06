#' Cast a value to the most appropriate type
#' @param value The value to cast.
#' @return The value casted to the most appropriate type.
cast_type <- function(value) {
  if (
    !is.na(
      posix <- suppressWarnings(
        lubridate::ymd_hms(
          value
        )
      )
    )
  ) {
    posix
  } else if (
    !is.na(
      date <- suppressWarnings(
        lubridate::ymd(
          value
        )
      )
    )
  ) {
    date
  } else if (
    value %in% c(
      "TRUE",
      "FALSE"
    )
  ) {
    as.logical(value)
  } else if (
    !is.na(
      num <- suppressWarnings(
        as.numeric(value)
      )
    )
  ) {
    num
  } else {
    value
  }
}
