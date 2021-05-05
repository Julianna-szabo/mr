#' Formats numbers as Hungarian Forint
#' @param x number
#' @return string
#' @export
#' @importFrom checkmate assert_number
#' @examples
#' forint(42)
#' forint(10000000.213214)
forint <- function(x) {
  assert_number(x)
  dollar(x, prefix = "", suffix = " Ft")
}
