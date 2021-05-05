#' Getting exchange rate between USD and HUF
#' @param retried number of times the function has failed
#' @return number
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error
#' @examples
#' get_usdhuf()
get_usdhuf <- function(retried = 0) {
  tryCatch({
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried^2)
    get_usdhuf(retried = retried + 1)
  })
  log_info('1 USD={usdhuf} HUF')
  usdhuf
}
