#' Cast a value to the most appropriate type
#' @param value The value to cast.
#' @return The value casted to the most appropriate type.
cast_type <- function(value) {
  # Try logical
  if (value %in% c("TRUE", "FALSE")) return(as.logical(value))

  # Try datetime only if it resembles a datetime (e.g. "2023-01-01 12:34:56")
  if (is.character(value) && grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}(:\\d{2})?", value)) {
    posix <- suppressWarnings(lubridate::ymd_hms(value))
    if (!is.na(posix)) return(posix)
  }

  # Try date only if it resembles a date (e.g. "2023-01-01")
  if (is.character(value) && grepl("^\\d{4}-\\d{2}-\\d{2}$", value)) {
    date <- suppressWarnings(lubridate::ymd(value))
    if (!is.na(date)) return(date)
  }

  # Try numeric
  num <- suppressWarnings(as.numeric(value))
  if (!is.na(num)) return(num)

  # Fallback
  return(value)
}
