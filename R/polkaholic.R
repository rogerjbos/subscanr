library(data.table)
library(httr)
library(jsonlite)

`%+%` <- function(a, b) paste0(a, b)

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
#' get_polkaholic_events("karura", module = "dex", call = 'Swap',
#' startDate='2022-07-01', endDate='2022-07-02', nobs = 5)
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_polkaholic_events <- function(chain, module, call, startDate, endDate, startBlock = NA, endBlock = NA, nobs = 1000) {

  # chain = "karura"; module = "dex"; call = 'Swap'; startDate='2022-07-01'; endDate='2022-07-03'
  # chain = "karura"; module = "dex"; call = 'Swap'; startBlock=1; endBlock=2208550
  stopifnot(nobs <= 100000)
  api_call <- 'api.polkaholic.io/search/events?limit=' %+% nobs
  baseurl <- paste0('https://', api_call)
  if (!is.na(startBlock) & !is.na(endBlock)) {
    body = list(chainIdentifier = chain, section = module, method = call, blockNumberStart = startBlock, blockNumberEnd = endBlock)
  } else {
    body = list(chainIdentifier = chain, section = module, method = call, dateStart = startDate, dateEnd = endDate)
  }

  r <- POST(url = baseurl,
            add_headers(polkaholic_api_header, 'Content-Type: application/json'),
            body = body,
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
  tmp[, time := as.POSIXct(blockTS, origin = "1970-01-01", tz = 'UTC')]
  tmp

}

# t1 <- get_polkaholic_events(chain = "karura", module = "dex", call = 'Swap', startBlock=1, endBlock=2208550, nobs = 10)
# t2 <- extract_polkaholic_events(t1)
# t2

#' Get events from the Polkadot blockchain from the Polkaholic api
#' https://docs.polkaholic.io
#'
#' @name extract_polkaholic_events
#' @title extract_polkaholic_events
#' @encoding UTF-8
#' @concept Extract events from the Polkadot blockchain from the Polkaholic api
#' @param dat data.table output from `get_polkaholic_events`
#'
#' @return data.table
#'
#' @examples
#' t1 <- get_polkaholic_events("karura", module = "dex", call = 'Swap',
#' startBlock=1, endBlock=2208550, nobs=10)
#' t2 <- extract_polkaholic_events(t1)
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
extract_polkaholic_events <- function(dat) {

  if (is.null(dat) || nrow(dat) < 1) {
    print("Supplied data is not suitable.  Did you run `get_polkaholic_events`?")
  } else if (dat$section =="dex" & dat$method == "Swap") {
    if (nrow(tmp) > 0) {
      d <- list()
      for (i in 1:nrow(tmp)) {
        # Pause to avoid 429 error
        if (i %% 5 == 0) Sys.sleep(1)
        d[[i]] <- get_polkaholic_transaction(tmp$extrinsicHash[i])
      }
      out <- rbindlist(d, fill = TRUE)
      return(out)
    }
  } else {
    print("Extractor not defined for section='" %+% dat$section[1] %+% "' and/or method='" %+% dat$method[1] %+% "'.")
  }
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
#' get_polkaholic_transaction(TxHash =
#' '0x8b91d038421d5aba4b3a651d70923fb0d41423b914d6dad910637aa2c9a2ad70')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_polkaholic_transaction <- function(TxHash) {

  # TxHash = '0xd2749f00dd78af2aae0ae77f2f48251735b8af8a7ad778f22a7b1de69c9b5c01'
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

  tmp2 <- tmp[events.section == 'dex']
  d <- list()
  for (i in 1:nrow(tmp2)) {
    ti <- tmp2[i]$events.decodedData[[1]]$data
    tokens <- ifelse(!is.na(ti[[2]]$token), ti[[2]]$token, names(ti[[2]][2]) %+% "://" %+%  ti[[2]][[2]])
    amount <- ti[[3]]
    while(length(amount) < 4) {
      tokens <- c(tokens, NA)
      amount <- c(amount, NA)
    }
    d[[i]] <- t(c(tokens, amount)) %>% as.data.table
  }
  out <- rbindlist(d) %>%
    as.data.frame %>%
    setnames(c("token" %+% rep(0:3), "amount" %+% rep(0:3)))

  out <- cbind(tmp2, out)
  out

}
