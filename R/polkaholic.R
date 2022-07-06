library(data.table)
library(httr)
library(jsonlite)

# MUST SET `polkaholic_api_key` with your private api key
if (!exists("polkaholic_api_key")) polkaholic_api_key <- ""
polkaholic_api_header <- 'Authorization: ' %+% polkaholic_api_key

#' Get supported chains from the Polkaholic api
#' https://docs.polkaholic.io
#'
#' @name get_polkaholic_chains
#' @title get_polkaholic_chains
#' @encoding UTF-8
#' @concept Get events from the Polkadot blockchain from the Polkaholic api
#' @param None
#'
#' @return data.table
#'
#' @examples
#' get_polkaholic_chains()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_polkaholic_chains <- function() {

  api_call <- 'api.polkaholic.io/chains'
  baseurl <- paste0('https://', api_call)

  r <- GET(url = baseurl,
            add_headers(polkaholic_api_header, 'Content-Type: application/json'),
            encode = "json")
  # r$status_code
  while(r$status_code != 200) {
    print("Error " %+% r$status_code)
    Sys.sleep(3)
    r <- POST(baseurl, body = body,
              add_headers(api_header, 'Content-Type: application/json'))
  }
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE) %>%
    as.data.table
  tmp

}

# tmp <- get_polkaholic_events(chain = "karura", module = "dex", call = 'Swap', startDate='2022-07-01', endDate='2022-07-02')


#' Get events from the Polkadot blockchain from the Polkaholic api
#' https://docs.polkaholic.io
#'
#' @name get_polkaholic_events
#' @title get_polkaholic_events
#' @encoding UTF-8
#' @concept Get events from the Polkadot blockchain from the Polkaholic api
#' @param chain string or integer specifying the target parachain
#' @param module string indicating which Polkadot module to use; leave blank for all.
#' @param call string indicating the module call (i.e. "Swap"); leave blank for all.
#' @param startDate string indicating the end date; leave blank for all.
#' @param endDate string indicating the start date; leave blank for all.
#'
#' @return data.table
#'
#' @examples
#' get_polkaholic_events(chain = "karura", module = "dex", call = 'Swap', startDate='2022-07-01', endDate='2022-07-02')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_polkaholic_events <- function(chain, module, call, startDate, endDate) {

  # chain = "karura"; module = "dex"; call = 'Swap'; startDate='2022-07-01'; endDate='2022-07-04'
  api_call <- 'api.polkaholic.io/search/events'
  baseurl <- paste0('https://', api_call)
  body = list(chainIdentifier = chain, section = module, method = call, dateStart = startDate, dateEnd = endDate)

  r <- POST(url = baseurl,
            add_headers(polkaholic_api_header, 'Content-Type: application/json'),
            body = body,
            encode = "json")
  # r$status_code
  while(r$status_code != 200) {
    print("Error " %+% r$status_code)
    Sys.sleep(3)
    r <- POST(baseurl, body = body,
              add_headers(api_header, 'Content-Type: application/json'))
  }
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE) %>%
    as.data.table
  # add a human-readable date
  tmp[, time := as.POSIXct(blockTS, origin = "1970-01-01", tz = 'UTC')]

  tmp

}

#' Get supported chains from the Polkaholic api
#' https://docs.polkaholic.io
#'
#' @name get_polkaholic_hash
#' @title get_polkaholic_hash
#' @encoding UTF-8
#' @concept Get hash lookup from the Polkadot blockchain from the Polkaholic api
#' @param TxHash string hash to lookup
#'
#' @return data.table
#'
#' @examples
#' get_polkaholic_hash(TxHash = '0x044e63a8548c51bac4f3cdf71cc3bc31e63b460f7652030604aee4361f1b1878')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_polkaholic_hash <- function(TxHash) {

  api_call <- 'api.polkaholic.io/hash/' %+% TxHash
  baseurl <- paste0('https://', api_call)

  r <- GET(url = baseurl,
           add_headers(polkaholic_api_header, 'Content-Type: application/json'),
           encode = "json")
  r$status_code
  while(r$status_code != 200) {
    print("Error " %+% r$status_code)
    Sys.sleep(3)
    r <- POST(baseurl, body = body,
              add_headers(api_header, 'Content-Type: application/json'))
  }
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE) %>%
    as.data.table
  # add a human-readable date
  tmp[, time := as.POSIXct(ts, origin = "1970-01-01", tz = 'UTC')]

  tmp

}

#' Get supported chains from the Polkaholic api
#' https://docs.polkaholic.io
#'
#' @name get_polkaholic_transaction
#' @title get_polkaholic_transaction
#' @encoding UTF-8
#' @concept Get events from the Polkadot blockchain from the Polkaholic api
#' @param TxHash string transaction hash
#'
#' @return data.table
#'
#' @examples
#' get_polkaholic_transaction(TxHash = '0x8b91d038421d5aba4b3a651d70923fb0d41423b914d6dad910637aa2c9a2ad70')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_polkaholic_transaction <- function(TxHash) {

  # TxHash = '0x1d6ef28701799a5a59ff4b4ff14cfe85e58140208af1c69f7c46bdf3830d081f'
  api_call <- 'api.polkaholic.io/tx/' %+% TxHash %+% '?decorate=true&extra=usd,address,related,data'
  baseurl <- paste0('https://', api_call)

  r <- GET(url = baseurl,
           add_headers(polkaholic_api_header, 'Content-Type: application/json'),
           encode = "json")
  r$status_code
  while(r$status_code != 200) {
    print("Error " %+% r$status_code)
    Sys.sleep(3)
    r <- POST(baseurl, body = body,
              add_headers(api_header, 'Content-Type: application/json'))
  }
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE) %>%
    as.data.table
  # add a human-readable date
  tmp[, time := as.POSIXct(ts, origin = "1970-01-01", tz = 'UTC')]

  tmp

}
