#' Function to simply format currency values
#'
#' Formatting will follow excel accounting style
#'
#' @param x numeric vector
#' @param digits numeric to denote number of decimal places
#' @param shown.in character to express value in thousands, millions or billions.
#' Accepts \code{c('k', 'm', 'b')}.
#'
#' @examples
#'
#' \dontrun{
#'
#' format_acct(100, digits = 0)
#'
#' library(dplyr)
#' mtcars %>% mutate(msrp = runif(nrow(.), 20000, 30000), msrp_format = format_acct(msrp, shown.in = 'k'))
#'
#' }
#'
#' @return character vector of \code{length(x)} with desired formatting
#' @export

format_acct <- function(x, digits = 2, shown.in = NULL) {

  if (!is.numeric(x)) stop('x must be numeric!')

  # If shown.in is not null then divide by appropriate value

  if (!is.null(shown.in)) {

    shown.in <- toupper(shown.in)

    if (shown.in == 'K') {
      x <- x/1e3
    } else if (shown.in == 'M') {
      x <- x/1e6
    } else if (shown.in == 'B') {
      x <- x/1e9
    } else {x}

  }

  # Round x to specified number of decimal places
  format_x <- format(round(abs(x), digits), nsmall = digits, big.mark = ',')
  # Remove any whitespace
  format_x <- gsub('\\s+', '', format_x)

  negative <- !is.na(x) & x < 0

  # Add dollar sign and ensure that round numbers (i.e. $100) still show desired decimal places
  # Negative values will have parens
  val <- paste0('$',
                ifelse(negative, '(', ''),
                format_x, shown.in,
                ifelse(negative, ')', ''))

  # Set NA values to NA
  is_na <- is.na(x)
  val[is_na] <- NA

  return(val)

}
