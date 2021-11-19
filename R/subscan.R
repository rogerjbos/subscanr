# Contact api@subscan.io to ket api key
library(httr)
library(jsonlite)
library(data.table)

#' Helper function wrapper to paste0()
#' @name `%+%`
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
`%+%` <- function(a, b) paste0(a, b)

# MUST SET `subscan_api_key` with your private api key
# Contact api@subscan.io to ket api key
api_header <- 'x-api-key=' %+% subscan_api_key


endpoint_list <- c("Polkadot","polkadot.api.subscan.io",
                   "Kusama","kusama.api.subscan.io",
                   "Darwinia","darwinia.api.subscan.io",
                   "Crust","crust.api.subscan.io",
                   "Acala Mandala","acala-testnet.api.subscan.io",
                   "Astar","astar.api.subscan.io",
                   "Bifrost Asgard","bifrost.api.subscan.io",
                   "Centrifuge","centrifuge.api.subscan.io",
                   "ChainX","chainx.api.subscan.io",
                   "Clover","clover.api.subscan.io",
                   "Clover Testnet","clover-testnet.api.subscan.io",
                   "Crust Maxwell","maxwell.api.subscan.io",
                   "Darwinia Crab","crab.api.subscan.io",
                   "DataHighway","datahighway.api.subscan.io",
                   "DataHighway","Harbour	datahighway-harbour.api.subscan.io",
                   "DeepBrain Chain","dbc.api.subscan.io",
                   "Dock","dock.api.subscan.io",
                   "Edgeware","edgeware.api.subscan.io",
                   "Equilibrium","equilibrium.api.subscan.io",
                   "Gateway","gateway-testnet.api.subscan.io",
                   "Karura","karura.api.subscan.io",
                   "Kulupu","kulupu.api.subscan.io",
                   "Khala","khala.api.subscan.io",
                   "KILT Peregrine","kilt-testnet.api.subscan.io",
                   "Laminar","TC2	laminar-testnet.api.subscan.io",
                   "Litentry","litentry.api.subscan.io",
                   "Manta","manta-testnet.api.subscan.io",
                   "Moonbase","moonbase.api.subscan.io",
                   "Moonriver","moonriver.api.subscan.io",
                   "Pangolin","pangolin.api.subscan.io",
                   "Pangoro","pangoro.api.subscan.io",
                   "Phala","Rorschach	phala.api.subscan.io",
                   "Polkadex","polkadex.api.subscan.io",
                   "Polymesh","polymesh.api.subscan.io",
                   "Polymesh-test","polymesh-testnet.api.subscan.io",
                   "Plasm","plasm.api.subscan.io",
                   "Reef","reef.api.subscan.io",
                   "Robonomics","robonomics-testnet.api.subscan.io",
                   "Rococo","rococo.api.subscan.io",
                   "Sakura","sakura.api.subscan.io",
                   "Shibuya","shibuya.api.subscan.io",
                   "Shiden","shiden.api.subscan.io",
                   "SORA","sora.api.subscan.io",
                   "Subgame","subgame.api.subscan.io",
                   "Stafi","stafi.api.subscan.io",
                   "Statemine","statemine.api.subscan.io",
                   "Uniarts","uniarts.api.subscan.io",
                   "Westend","westend.api.subscan.io")
endpoints <- matrix(endpoint_list, ncol = 2, byrow = TRUE) %>%
  as.data.table %>%
  setnames(c("network_name","api_host"))


#' Get events from the Polkadot blockchain from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_events
#' @title get_subscan_events
#' @encoding UTF-8
#' @concept Get events from the Polkadot blockchain from the Subscan api
#' @param nobs integer how many transactions to pull
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#' @param module string indicating which module to pull; leave blank for all.
#' @param call string indicating which function call to pull; leave blank for all.
#'
#' @return data.table
#'
#' @examples
#' tmp <- get_subscan_events(nobs = 111); dim(tmp$core_data)
#' tmp <- get_subscan_events(nobs = 10, network = 'Karura', module = 'dex', call = 'Swap')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_events <- function(nobs = 10, network = 'Karura', module = '', call = '') {

  # nobs = 10; network = 'Karura'; module = 'dex'; call = 'Swap'; page = 1

  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/scan/events'
  baseurl <- paste0('https://', api_host, api_call)
  fname <- network %+% "_events.csv"

  # each `page` pulls in 100 rows, so calculate how many pages you need to pull
  last_page <- ceiling(max(1, (nobs /100)))

  page_list <-list()
  params_list <-list()
  for (page in 1:last_page) {

    body <- '{"row": ' %+% min(100, nobs) %+% ',"page": ' %+% page %+% ',"module": "' %+% module %+% '","call": "' %+% call %+% '"}'

    r <- POST(baseurl, body = body,
              add_headers(api_header, 'Content-Type=application/json'))
    stop_for_status(r)
    tmp <- content(r, as="text", encoding="UTF-8") %>%
      fromJSON(flatten=TRUE)

    core_data <- tmp$data$events %>%
      as.data.table
    params <- core_data$params
    core_data <- core_data[, params := NULL]

    print(nrow(core_data) %+% " rows for page " %+% page %+% " had " %+% tmp$message %+% " at " %+% Sys.time())

    if (nrow(core_data) > 0) {
      # add a human-readable date
      core_data[, time := as.POSIXct(block_timestamp, origin = "1970-01-01", tz = 'UTC')]

      if (module == 'dex' & call == 'Swap') {

        d <- list()
        for (i in 1:length(params)) {
          ti <- fromJSON(params[[i]], flatten=TRUE)

          if (length(ti$value[[3]]) == 2) {
            amt_0 <- as.numeric(ti$value[[3]][1]) / 1e12
            amt_1 <- as.numeric(ti$value[[3]][2]) / 1e12
            amt_2 <- NA
            amt_3 <- NA
          } else if (length(ti$value[[3]]) == 3) {
            amt_0 <- as.numeric(ti$value[[3]][1]) / 1e12
            amt_1 <- as.numeric(ti$value[[3]][2]) / 1e12
            amt_2 <- as.numeric(ti$value[[3]][3]) / 1e12
            amt_3 <- NA
          } else if (length(ti$value[[3]]) == 4) {
            amt_0 <- as.numeric(ti$value[[3]][1]) / 1e12
            amt_1 <- as.numeric(ti$value[[3]][2]) / 1e12
            amt_2 <- as.numeric(ti$value[[3]][3]) / 1e12
            amt_3 <- as.numeric(ti$value[[3]][4]) / 1e12
          } else if (nrow(ti) > 4) {
            stop("more than 4 pairs")
          }

          d[[i]] <- data.table(account = ti$value[[1]],
                     id_0 = ti$value[[2]]$Token[1],
                     id_1 = ti$value[[2]]$Token[2],
                     id_2 = ti$value[[2]]$Token[3],
                     id_3 = ti$value[[2]]$Token[4],
                     amt_0,
                     amt_1,
                     amt_2,
                     amt_3)

        }
        params_out <- rbindlist(d)

        all_data <- data.table(core_data, params_out)
        page_list[[page]] <- all_data

      } else {

        page_list[[page]] <- core_data
        params_list[[page]] <- params

      }
    }

    # write the data to a file as backup
    if (page %% 100 == 0) {
      rd = rbindlist(page_list)
      fwrite(rd, fname, append = FALSE)
    }

    # adjust for how many transaction left to pull
    nobs <- nobs - nrow(core_data)
  }
  # write the data to a file as backup
  rd = rbindlist(page_list)
  fwrite(rd, fname, append = FALSE)

  if (module == 'dex' & call == 'Swap') return(rd)
  list(rd_data, params = params_list)

}

#' Get network metadata from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_metadata
#' @title get_subscan_metadata
#' @encoding UTF-8
#' @concept Get network metadata from the Subscan api
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#'
#' @return list
#'
#' @examples
#' get_subscan_metadata()
#' get_subscan_metadata(network = 'Darwinia')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_metadata <- function(network = 'Karura') {

  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/scan/metadata'
  baseurl <- paste0('https://', api_host, api_call)
  r <- POST(baseurl,
            add_headers(api_header, 'Content-Type=application/json'))
  stop_for_status(r)

  content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE)
}

#' Get all events for a specific extrinsic id from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_extrinsic
#' @title get_subscan_extrinsic
#' @encoding UTF-8
#' @concept Get all events for a specific extrinsic id from the Subscan api
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#' @param extrinsic string extrinsic id to pull.
#'
#' @return list
#'
#' @examples
#' get_subscan_extrinsic(extrinsic = '398539-2')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_extrinsic <- function(network = 'Karura', extrinsic = '398539-2') {

  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/scan/extrinsic'
  baseurl <- paste0('https://', api_host, api_call)
  r <- POST(baseurl,
            add_headers(api_header, 'Content-Type=application/json'),
            body = '{"extrinsic_index": "' %+% extrinsic %+% '"}')
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE)
  tmp$data

}

#' Get price for a specific block time from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_price
#' @title get_subscan_price
#' @encoding UTF-8
#' @concept Get price for a specific block time from the Subscan api
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#' @param time integer block time or unix time.
#'
#' @return list
#'
#' @examples
#' get_subscan_price(extrinsic = '398539-2')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_price <- function(network = 'Karura', time = 957105) {

  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/open/price'
  baseurl <- paste0('https://', api_host, api_call)
  r <- POST(baseurl,
            add_headers(api_header, 'Content-Type=application/json'),
            body = '{"time": ' %+% time %+% '}')
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE)
  tmp$data

}

#' Get average price for a date range from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_price_history
#' @title get_subscan_price_history
#' @encoding UTF-8
#' @concept Get average price for a date range from the Subscan api
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#' @param start date start time.
#' @param end date end time.
#'
#' @return list
#'
#' @examples
#' get_subscan_price_history(network = 'Darwinia', start = '2021-11-01', end = '2021-11-02')
#' get_subscan_price_history(network = 'Karura', start = '2021-11-01', end = '2021-11-02')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_price_history <- function(network = 'Karura', start = '2021-11-01', end = '2021-11-02') {

  # network= "Polkadot"
  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/scan/price/history'
  baseurl <- paste0('https://', api_host, api_call)
  r <- POST(baseurl,
            add_headers(api_header, 'Content-Type=application/json'),
            body = '{"start": "' %+% start %+% '","end": "' %+% end %+% '"}')
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE)
  tmp$data

}

#' Get price for a specific block time from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_price
#' @title get_subscan_price
#' @encoding UTF-8
#' @concept Get price for a specific block time from the Subscan api
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#' @param time integer block time or unix time.
#'
#' @return list
#'
#' @examples
#' get_subscan_price(extrinsic = '398539-2')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_currencies <- function(network = 'Karura') {

  # network="Polkadot"
  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/open/currencies'
  baseurl <- paste0('https://', api_host, api_call)
  r <- POST(baseurl,
            add_headers(api_header, 'Content-Type=application/json'))
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE)
  tmp$data

}

#' Get the amount of the target currency base on the amount of source currency given historical price from the Subscan api
#' https://docs.api.subscan.io
#'
#' @name get_subscan_price_converter
#' @title get_subscan_price_converter
#' @encoding UTF-8
#' @concept Get the amount of the target currency base on the amount of source currency given historical price from the Subscan api
#' @param network string indicating which Polkadot endpoint to use; defaults to 'Karura'.
#' @param start date start time.
#' @param end date end time.
#'
#' @return list
#'
#' @examples
#' get_subscan_price_converter(network = 'Polkadot', time = 957105, value = 1000, from = 'USD', quote = 'DOT')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_price_converter <- function(network = 'Karuka', time = 957105, value = 1000, from = 'USD', quote = 'DOT') {

  # network = "Polkadot"
  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/open/price_converter'
  baseurl <- paste0('https://', api_host, api_call)
  r <- POST(baseurl,
            add_headers(api_header, 'Content-Type=application/json'),
            body = '{"time": ' %+% time %+% ',
                    "value": ' %+% value %+% ',
                    "from": "' %+% from %+% '",
                    "quote": "' %+% quote %+% '"}')
  stop_for_status(r)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE)
  tmp$data

}
