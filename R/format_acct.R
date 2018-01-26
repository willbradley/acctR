#' Function to simply format currency values
#'
#' Formatting will follow excel accounting style
#'
#' @param x numeric vector
#' @param digits numeric to denote number of decimal places
#' @param shown.in character to express value in thousands, millions or billions
#'
#' @examples
#'
#' \dontrun{
#'
#' format_acct(100, digits = 0)
#'
#' mtcars %>% mutate(msrp = runif(nrow(.), 20000, 30000), msrp_format = format_acct(msrp))
#'
#' }
#'
#' @export

format_acct <- function(x, digits = 2, shown.in = NULL) {

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

  format_x <- format(round(abs(x), digits), nsmall = digits, big.mark = ',')
  format_x <- gsub('\\s+', '', format_x)

  negative <- x < 0

  val <- paste0('$',
                ifelse(negative, '(', ''),
                format_x, shown.in,
                ifelse(negative, ')', ''))

  return(val)

}
