#' Getting exchange rate between USD and HUF
#' @param retried number of times the function has failed
#' @return number
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
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

#' Getting exchange rate between USD and HUF for 30 days
#' @param start_date date
#' @param end_date date
#' @inheritParams  get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom logger log_error
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_usdhuf_2()

get_usdhuf_2 <- function(start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = "USD",
        symbols = "HUF"
      )
    )
    exchange_rates <- content(response)$rates

    usdhuf_rates <- data.table(
      date = as.Date(names(exchange_rates)),
      usdhuf = as.numeric(unlist(exchange_rates))
    )
    assert_numeric(usdhuf_rates$usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried^2)
    get_usdhuf_2(start_date, end_date, retried = retried + 1)
  })
  usdhuf_rates
}

