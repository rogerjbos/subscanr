# remotes::install_github("ropensci/ghql") # if package is not already installed
#' library(jsonlite)
#' library(data.table)
#' library(ghql)
#' x <- GraphqlClient$new()
#'
#' #' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' #' @export
#' mysort <- function(a, b) {
#'   ifelse(a < b, a %+% ":" %+% b, b %+% ":" %+% a)
#' }

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
myPath <- function(path) {
  # path <- res$tradePath
  path0 <- list()
  path1 <- list()
  path2 <- list()
  path <- strsplit(path, ",")
  for (i in 1:length(path)) {
    path0[[i]] <- mysort(fixToken(path[[i]][1]), fixToken(path[[i]][2]))
    path1[[i]] <- ifelse(path[[i]][3] == "", "", mysort(fixToken(path[[i]][2]), fixToken(path[[i]][3])))
    path2[[i]] <- ifelse(path[[i]][4] == "", "", mysort(fixToken(path[[i]][3]), fixToken(path[[i]][4])))
  }
  list(path0=path0, path1=path1, path2=path2)
}

# Helper function to concat
`%+%` <- function(a, b) paste0(a, b)

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
fixToken <- function(x) {
  x <- gsub('fa://0', 'RMRK', x)
  x <- gsub('ForeignAsset://0', 'RMRK', x)
  x <- gsub('lc://13', 'LCDOT', x)
  x <- gsub('LiquidCrowdloan://13', 'LCDOT', x)
  x <- gsub('sa://0', 'taiKSM', x)
  x <- gsub('StableAssetPoolToken://0', 'taiKSM', x)
  x
}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
tokens <- as.data.table(rbind(c("ACA", "Acala", 12),
                c("AUSD","Acala Dollar", 12),
                c("taiKSM","Taiga KSM", 12),
                c("RMRK","RMRK", 10),
                c("DOT","Polkadot", 10),
                c("LCDOT","Liquid Crowdloan DOT", 10),
                c("LDOT","Liquid DOT", 10),
                c("RENBTC","Ren Protocol BTC", 8),
                c("CASH","Compound CASH", 8),
                c("KAR","Karura", 12),
                c("KUSD","Karura Dollar", 12),
                c("KSM","Kusama", 12),
                c("LKSM","Liquid KSM", 12),
                c("TAI","Taiga", 12),
                c("BNC","Bifrost Asgard", 12),
                c("VSKSM","Bifrost Voucher Slot KSM", 12),
                c("PHA","Phala Native Token", 12),
                c("KINT","Kintsugi Native Token", 12),
                c("KBTC","Kintsugi Wrapped BTC", 8)))
setnames(tokens, c("Token","Name","decimals"))
try(tokens[, divisor := (as.numeric(substr(as.character(1e20), 1, as.numeric(decimals) + 1))), by = Token], silent = TRUE)

# Query function for very simple queries
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_query <- function(url, query) {

  # url <- 'https://api.subquery.network/sq/AcalaNetwork/karura-tokens'
  # query <- 'query { accountBalances (first: 5) { nodes { id accountId tokenId total } } }'

  method = "test"
  cli <- GraphqlClient$new(url)
  qry <- Query$new()
  qry$query(method, query)

  result <- cli$exec(qry$queries[[method]])  %>%
    fromJSON(flatten=TRUE)
  result

}


# Helper function used by all the other query calls
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_graph <- function(endpoint, method, edges, window, filter = "timestamp") {


  cli <- GraphqlClient$new(url = endpoint)
  mindate <- today(tzone = 'UTC') - window
  if (filter == "") {
    filterStr <- ' ( '
  } else if (filter == "timestamp") {
    filterStr <- ' (filter: {timestamp: {greaterThanOrEqualTo: "' %+% mindate %+% '"}} '
  } else if (filter == "date") {
    filterStr <- ' (filter: {date: {greaterThanOrEqualTo: "' %+% mindate %+% '"}} '
  } else {
    filterStr <- ' (' %+% filter
  }
  cursor <- ''
  resList <- list()
  for (i in 1:1000) {
    if (cursor == '') {
      cursorStr <- 'first:100'
    } else {
      cursorStr <- 'first:100 after:"' %+% cursor %+% '"'
    }
    qry <- Query$new()
    qry$query(method, '
    {
      query {
        ' %+% method %+% filterStr %+% cursorStr %+% ') {
          totalCount
          edges {
            node { ' %+% edges %+% '}
            cursor
          }
          pageInfo {
            endCursor
            hasNextPage
          }
        }
      }
    }')
    result <- cli$exec(qry$queries[[method]])  %>%
      fromJSON(flatten=TRUE)
    cursor <- result$data$query[[method]]$pageInfo$endCursor
    res <- as.data.table(result$data$query[[method]]$edges)
    if (nrow(res) == 0) break
    res[, cursor := NULL]

    print(i %+% " " %+% nrow(res))
    resList[[i]] <- res
    if (result$data$query[[method]]$pageInfo$hasNextPage == FALSE) break
  }
  res <- rbindlist(resList)
  setnames(res, old = names(res), new = gsub("node.", "", names(res)))

  res

}



#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getDailyPools_acala_dex <- function(network, window) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dailyPools"
  edges <- "timestamp token0 {id} token1 {id} feeRateUSD dailyTradeVolumeUSD totalTVL txCount"
  res <- get_graph(endpoint, method, edges, window)

  res[, date := as.Date(timestamp)]
  setorder(res, date)
  setnames(res,
           c("token0.id","token1.id","dailyTradeVolumeUSD","totalTVL","feeRateUSD"),
           c("token0Id", "token1Id", "volumeUSD", "tvlUSD", "feeUSD"))

  # Replace foreign assets
  res[, token0Id := fixToken(token0Id)]
  res[, token1Id := fixToken(token1Id)]

  # Normalize pairs
  res[, pair := paste0(token0Id %+% ":" %+% token1Id)]
  res[token1Id < token0Id, pair := paste0(token1Id %+% ":" %+% token0Id)]

  res[, feeUSD := as.numeric(feeUSD) / 1e18]
  res[, volumeUSD := as.numeric(volumeUSD) / 1e18]
  res[, tvlUSD := as.numeric(tvlUSD) / 1e18]

  res

}


#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLiquidity_acala <- function(endpoint, window) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dexActions"
  edges <- "timestamp id nodeId accountId type token0Id token1Id token0Amount token1Amount volumeUSD"
  res <- get_graph(endpoint, method, edges, window)

  res[, date := as.Date(timestamp)]
  setorder(res, date)

  # Replace foreign assets
  res[, token0Id := fixToken(token0Id)]
  res[, token1Id := fixToken(token1Id)]

  # Normalize pairs
  res[, pair := paste0(token0Id %+% ":" %+% token1Id)]
  res[token1Id < token0Id, pair := paste0(token1Id %+% ":" %+% token0Id)]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLoansCollateralParams_acala_loan <- function(network) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/acala-loan-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/karura-loan-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "collateralParams"
  filter <- ""
  edges <- "collateral {id} maximumTotalDebitValue interestRatePerSec liquidationRatio
              liquidationPenalty requiredCollateralRatio"
  res <- get_graph(endpoint, method, edges, filter = "", window=1)

  # Replace foreign assets
  res[, collateral.id := fixToken(collateral.id)]
  res <- merge(res, tokens, by.x='collateral.id', by.y='Token')

  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, maximumTotalDebitValue := as.numeric(maximumTotalDebitValue) / as.numeric(adj)]
  res[, liquidationRatio := as.numeric(liquidationRatio) / 1e18]
  res[, liquidationPenalty := as.numeric(liquidationPenalty) / 1e18]
  res[, requiredCollateralRatio := as.numeric(requiredCollateralRatio) / 1e18]
  res[, APR := (as.numeric(interestRatePerSec) / 1e18 + 1) ** (365 * 86400) - 1]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLoansDailyPositions_acala_loan <- function(network, window) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/acala-loan-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/karura-loan-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dailyPositions"
  filter <- "timestamp"
  edges <- "id owner {id} collateral {id} depositAmount debitAmount depositVolumeUSD debitVolumeUSD
               depositChangedUSD debitChangedUSD debitExchangeRate timestamp txCount"
  res <- get_graph(endpoint, method, edges, window)

    # endpoint <-loan_endpoint
  method <- 'dailyPositions'; maxn <- 1000

  cli <- GraphqlClient$new(url = endpoint)
  mindate <- today(tzone = 'UTC') - window

  cursor <- ''
  resList <- list()
  for (i in 1:maxn) {
    if (cursor == '') {
      cursorStr <- 'first:100'
    } else {
      cursorStr <- 'first:100 after:"' %+% cursor %+% '"'
    }
    qry <- Query$new()
    qry$query(method, '
      {
        query {
          ' %+% method %+% ' (filter: {timestamp: {greaterThanOrEqualTo: "' %+% mindate %+% '"}} ' %+% cursorStr %+% ') {
            totalCount
            edges {
              node {
               id owner {id} collateral {id} depositAmount debitAmount depositVolumeUSD debitVolumeUSD
               depositChangedUSD debitChangedUSD debitExchangeRate timestamp txCount
              }
              cursor
            }
            pageInfo {
              endCursor
              hasNextPage
            }
          }
        }
      }')
    result <- cli$exec(qry$queries[[method]])  %>%
      fromJSON(flatten=TRUE)
    cursor <- result$data$query[[method]]$pageInfo$endCursor
    res <- as.data.table(result$data$query[[method]]$edges)
    res[, cursor := NULL]

    print(i %+% " " %+% nrow(res))
    resList[[i]] <- res
    if (result$data$query[[method]]$pageInfo$hasNextPage == FALSE) break
  }
  res <- rbindlist(resList) %>%
    setnames(names(res), gsub("node.", "", names(res)))
  res[, Date := as.Date(timestamp)]
  res[, collateral.id := fixToken(collateral.id)]
  res <- merge(res, tokens, by.x='collateral.id', by.y='Token')

  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, depositAmount := as.numeric(depositAmount) / adj]
  res[, debitAmount := as.numeric(debitAmount) / adj]

  res[, depositVolumeUSD := as.numeric(depositVolumeUSD) / 1e18]
  res[, debitVolumeUSD := as.numeric(debitVolumeUSD) / 1e18]
  res[, depositChangedUSD := as.numeric(depositChangedUSD) / 1e18]
  res[, debitChangedUSD := as.numeric(debitChangedUSD) / 1e18]
  res[, debitExchangeRate := as.numeric(debitExchangeRate) / 1e18]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLoansDailyCollateral_acala_loan <- function(network, window) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/acala-loan-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/karura-loan-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dailyCollaterals"
  filter <- "timestamp"
  edges <- "id collateral {id} depositAmount debitAmount depositVolumeUSD debitVolumeUSD
            depositChangedUSD debitChangedUSD debitExchangeRate timestamp txCount"
  res <- get_graph(endpoint, method, edges, window)

  res[, Date := as.Date(timestamp)]
  res[, collateral.id := fixToken(collateral.id)]
  res <- merge(res, tokens, by.x='collateral.id', by.y='Token')

  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, depositAmount := as.numeric(depositAmount) / adj]
  res[, debitAmount := as.numeric(debitAmount) / adj]

  res[, depositVolumeUSD := as.numeric(depositVolumeUSD) / 1e18]
  res[, debitVolumeUSD := as.numeric(debitVolumeUSD) / 1e18]
  res[, depositChangedUSD := as.numeric(depositChangedUSD) / 1e18]
  res[, debitChangedUSD := as.numeric(debitChangedUSD) / 1e18]
  res[, debitExchangeRate := as.numeric(debitExchangeRate) / 1e18]
  res

}


#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getSwaps_acala_dex <- function(network, window, block = NULL) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "swaps"
  edges <- "id address {id} pool {id}  token0 {id} token1 {id} token0InAmount token1OutAmount tradePath price0 price1
            block {id} extrinsic {id} timestamp"
  if (!is.null(block)) {
    filter <- 'filter: {blockId: {equalTo: "' %+% block %+% '"}} '
    res <- get_graph(endpoint, method, edges, window, filter)
  } else {
    res <- get_graph(endpoint, method, edges, window)
  }

  # if (substr(max(res$timestamp), 12, 13) < 23) {
  #   maxdate <- as.Date(max(res$timestamp))-1
  # } else {
  #   maxdate <- as.Date(max(res$timestamp))
  # }
  # res <- res[timestamp <= maxdate]

  res[, date := as.Date(timestamp)]
  setorder(res, timestamp)

  names(res) <- gsub(".id","", names(res))


  # Replace foreign assets
  res[, token0 := fixToken(token0)]
  res[, token1 := fixToken(token1)]
  res[, block_num := as.numeric(block)]

  # Normalize pairs
  res[, pair := paste0(token0 %+% ":" %+% token1)]
  res[token1 < token0, pair := paste0(token1 %+% ":" %+% token0)]

  z <- myPath(res$tradePath)
  res[, path0 := z$path0][, path1 := z$path1][, path2 := z$path2]
  res[, exclude := token0 == token1]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getSwaps_acala <- function(network, window, block = NULL) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dexActions"
  edges <- "timestamp id accountId token0Id token1Id  volumeUSD data"
  if (!is.null(block)) {
    filter <- 'filter: {id: {equalTo: "' %+% block %+% '"}} '
    res <- get_graph(endpoint, method, edges, window, filter)
  } else {
    res <- get_graph(endpoint, method, edges, window)
  }

  if (substr(max(res$timestamp), 12, 13) < 23) {
    maxdate <- as.Date(max(res$timestamp))-1
  } else {
    maxdate <- as.Date(max(res$timestamp))
  }

  res <- res[timestamp <= maxdate]
  res[, date := as.Date(timestamp)]
  setorder(res, timestamp)

  # Replace foreign assets
  res[, token0Id := fixToken(token0Id)]
  res[, token1Id := fixToken(token1Id)]

  # Normalize pairs
  res[, pair := paste0(token0Id %+% ":" %+% token1Id)]
  res[token1Id < token0.id, pair := paste0(token1Id %+% ":" %+% token0Id)]
  res[, exclude := token0Id == token1Id]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLiquidateUnsafeCDP_acala_loan <- function(network, window) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/acala-loan-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/rogerjbos/karura-loan-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "liquidUnsaves"
  edges <- "id sender {id} owner {id} collateral {id} collateralAmount collateralVolumeUSD
            badDebitVolumeUSD liquidationStrategy price debitExchangeRate block {id} timestamp"
  res <- get_graph(endpoint, method, edges, window)

  res[, Date := as.Date(timestamp)]
  res[, collateral.id := fixToken(collateral.id)]
  res <- merge(res, tokens, by.x='collateral.id', by.y='Token')

  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, collateralAmount := as.numeric(collateralAmount)]
  res[, price := as.numeric(price)]
  res[, debitExchangeRate := as.numeric(debitExchangeRate)]
  res[, collateralFloat := collateralAmount / adj]

  res[, collateralVolumeUSD := as.numeric(collateralVolumeUSD) / 1e18]
  res

}
