#' sine sad
#'
#' @param x x
#' @param P freq
#'
#' @export
sinpi <- function(x, P) {
  sin(2 * pi * x * (1/P))
}

#' cosine sad
#'
#' @param x x
#' @param P freq
#'
#' @export
cospi <- function(x, P) {
  cos(2 * pi * x * (1/P))
}
