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
