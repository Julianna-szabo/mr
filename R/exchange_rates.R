#' Getting exchange rate between USD and HUF for 30 days
#' @param base_currency character
#' @param end_currency character
#' @inheritParams  get_usdhuf_2
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom logger log_error
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_exchange_rate()

get_exchange_rate <- function(base_currency, end_currency, start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = base_currency,
        symbols = end_currency
      )
    )
    exchange_rates <- content(response)$rates

    rates <- data.table(
      date = as.Date(names(exchange_rates)),
      rate = as.numeric(unlist(exchange_rates))
    )
    assert_numeric(rates$rate, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried^2)
    get_exchange_rate(base_currency, end_currency, start_date, end_date, retried = retried + 1)
  })
  rates
}


get_exchange_rate("USD", "HUF")
