#' Roll a Die
#'
#' @param pips Integer. Max value of an integer series starting at 1 from which
#'     to sample.
#' @param n  Integer. Number of samples.
#'
#' @noRd
.roll <- function(pips = 6L, n = 1L) {

  if (!is.integer(n) | !is.integer(pips)) {
    stop("Arguments 'n' and 'pips' must each be a single integer.")
  }

  sample(x = seq(pips), size = n, replace = TRUE)

}
