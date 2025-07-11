
.find_non_numeric <- function(.data) {
  .data %>%
    filter(
      if_any(everything(),
             ~ !is.na(.) & is.na(suppressWarnings(as.numeric(.))))
    )
}