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

  x <- gsub('fa://1', 'ARIS', x)
  x <- gsub('ForeignAsset://1', 'ARIS', x)

  x <- gsub('fa://2', 'QTZ', x)
  x <- gsub('ForeignAsset://2', 'QTZ', x)

  x <- gsub('fa://3', 'MOVRZ', x)
  x <- gsub('ForeignAsset://3', 'MOVR', x)

  x <- gsub('fa://4', 'HKO', x)
  x <- gsub('ForeignAsset://4', 'HKO', x)

  x <- gsub('fa://5', 'CSM', x)
  x <- gsub('ForeignAsset://5', 'CSM', x)

  x <- gsub('fa://6', 'KICO', x)
  x <- gsub('ForeignAsset://6', 'KICO', x)

  x <- gsub('fa://7', 'USDT', x)
  x <- gsub('ForeignAsset://7', 'USDT', x)

  x <- gsub('fa://8', 'TEER', x)
  x <- gsub('ForeignAsset://8', 'TEER', x)

  x <- gsub('fa://9', 'NEER', x)
  x <- gsub('ForeignAsset://9', 'NEER', x)

  x <- gsub('fa://10', 'KMA', x)
  x <- gsub('ForeignAsset://10', 'KMA', x)

  x <- gsub('fa://11', 'BSX', x)
  x <- gsub('ForeignAsset://11', 'BSX', x)

  x <- gsub('fa://12', 'AIR', x)
  x <- gsub('ForeignAsset://12', 'AIR', x)

  x <- gsub('fa://13', 'CRAB', x)
  x <- gsub('ForeignAsset://13', 'CRAB', x)

  x <- gsub('fa://14', 'GENS', x)
  x <- gsub('ForeignAsset://14', 'GENS', x)

  x <- gsub('fa://15', 'EQD', x)
  x <- gsub('ForeignAsset://15', 'EQD', x)

  x <- gsub('lc://13', 'LCDOT', x)
  x <- gsub('LiquidCrowdloan://13', 'LCDOT', x)

  x <- gsub('sa://0', 'taiKSM', x)
  x <- gsub('StableAssetPoolToken://0', 'taiKSM', x)

  x <- gsub('fa%3A%2F%2F2', 'QTZ', x)
  x <- gsub('fa%3A%2F%2F0', 'RMRK', x)
  x <- gsub('fa%3A%2F%2F1', 'ARIS', x)
  x <- gsub('fa%3A%2F%2F5', 'CSM', x)
  x <- gsub('sa%3A%2F%2F0', 'taiKSM', x)
  x <- gsub('lc%3A%2F%2F13', 'LCDOT', x)

  x <- gsub('KUSD', 'AUSD', x)
  x
}


#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
tokens <- as.data.table(rbind(c("3USD", "Taiga 3USD", 12),
                              c("ACA", "Acala", 12),
                              c("AIR","Altair", 18),
                              c("ARIS","PolarisDAO", 8),
                              c("AUSD","Acala Dollar", 12),
                              c("BNC","Bifrost Asgard", 12),
                              c("BSX","Basilisk", 12),
                              c("CASH","Compound CASH", 8),
                              c("CRAB","Crab Parachain Token", 18),
                              c("CSM","Crust Storage Market", 12),
                              c("taiKSM","Taiga KSM", 12),
                              c("DOT","Polkadot", 10),
                              c("DOT (on Homa)","Polkadot", 10),
                              c("EQD","Equilibrium USD", 9),
                              c("GENS","Genshiro Native Token", 9),
                              c("HKO","Heiko", 12),
                              c("KAR","Karura", 12),
                              c("KBTC","Kintsugi Wrapped BTC", 8),
                              c("KICO","KICO", 14),
                              c("KINT","Kintsugi Native Token", 12),
                              c("KMA","Calamri", 12),
                              c("KSM","Kusama", 12),
                              c("KSM (on Homa)","Kusama", 12),
                              c("KUSD","Karura Dollar", 12),
                              c("LCDOT","Liquid Crowdloan DOT", 10),
                              c("LDOT","Liquid DOT", 10),
                              c("LKSM","Liquid KSM", 12),
                              c("MOVR","Moonriver", 18),
                              c("NEER","Metaverse.Network Pioneer", 18),
                              c("PHA","Phala Native Token", 12),
                              c("QTZ","Quartz", 12),
                              c("RMRK","Remark", 10),
                              c("RENBTC","Ren Protocol BTC", 8),
                              c("TAI","Taiga", 12),
                              c("taiKSM","Taiga KSM", 12),
                              c("TEER","Integritee Trusted Execution Environment", 12),
                              c("USDC","USD Coin (Wormhole)", 6),
                              c("USDT","Tether USD", 6),
                              c("VSKSM","Bifrost Voucher Slot KSM", 12)))
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
get_graph <- function(endpoint, method, edges, window, filter = "timestamp", endpage = 1000) {


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
  for (i in 1:endpage) {
    if (cursor == '') {
      cursorStr <- ' first:100'
    } else {
      cursorStr <- ' first:100 after:"' %+% cursor %+% '"'
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
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
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
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dailyPositions"
  filter <- "timestamp"
  edges <- "id owner {id} collateral {id} depositAmount debitAmount depositVolumeUSD debitVolumeUSD
               depositChangedUSD debitChangedUSD debitExchangeRate timestamp txCount"
  res <- get_graph(endpoint, method, edges, window)

  res[, Date := as.Date(timestamp)]
  res[, collateral.id := fixToken(collateral.id)]
  res <- merge(res, subscanr::tokens, by.x='collateral.id', by.y='Token', allow.cartesian=TRUE)

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
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
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
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
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


#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getAccountBalance_moonbeam_token <- function(network, window = 1, filter = '', endpage = 2000) {

  if (tolower(network) == 'moonbeam') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-tokens"
    #
  } else if (tolower(network) == 'moonriver') {
    endpoint <- "https://api.subquery.network/sq/bizzyvinci/moonriver"
    # https://github.com/bizzyvinci/moonriver-unified-subquery
    token_id <- "KSM"
  } else {
    stop("Network not found; must be one of 'moonbeam' or 'moonriver'")
  }

  if (filter == "") filter <- ' filter: {freeBalance: {greaterThan: "0"}} '
  method <- "accounts"
  edges <- "id freeBalance"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)
  # unique(res$tokenId)

  res[, tokenId := token_id]
  res <- merge(res, tokens, by.x='tokenId', by.y='Token')
  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, total := as.numeric(total) / adj]
  res[, Name := NULL]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getAccountBalance_acala_token <- function(network, window, filter = '', endpage = 2000) {

  # query {\n        accountBalances (filter: {tokenId: {in: [\"DOT\",\"LDOT\"]}, total: {greaterThan: \"0\"}}  first:100) {\n          totalCount\n          edges {\n            node { accountId tokenId total}\n            cursor\n          }\n          pageInfo {\n            endCursor\n            hasNextPage\n          }\n        }\n      }\n    }"

  # network="acala"; window = 1; filter = 'filter: {tokenId: {in: ["DOT","LDOT"]}, total: {greaterThan: "0"}} '; endpage = 2e9

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-tokens__QWNhb"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-tokens__QWNhb"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "accountBalances"
  edges <- "accountId tokenId total"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)
  # unique(res$tokenId)

  res[, tokenId := subscanr::fixToken(tokenId)]
  res <- merge(res, tokens, by.x='tokenId', by.y='Token')
  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, total := as.numeric(total) / adj]
  res[, Name := NULL]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getPositions_acala_loan <- function(network, window, filter, endpage = 2000) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  # filter <- 'filter: {collateralId: {in: ["ACA","DOT","LDOT","KSM","LKSM"]}} '; endpage <- 2
  method <- "positions"
  edges <- "ownerId collateralId txCount depositAmount debitAmount"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  res[, collateralId := fixToken(collateralId)]
  res <- merge(res, tokens, by.x='collateralId', by.y='Token')
  res[, Name := NULL]

  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, depositAmount := as.numeric(depositAmount) / adj]
  res[, debitAmount := as.numeric(debitAmount) / adj]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getAccounts_acala <- function(network, window, filter = '', endpage = 2000) {

  # network="acala"; window = 1; filter = ''; endpage = 2

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "accounts"
  edges <- "id txCount createAtBlockId"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getPoolStats_acala <- function(network, window = 1) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "pools"
  edges <- "id,
            token0 {decimal, name}
            token1 {decimal, name}
            token0Amount
            token1Amount
            tvlUSD
            dayData(orderBy:DATE_DESC,first:7) {
                nodes {
                    date
                    tvlUSD
                    volumeUSD
                }
            }"
  res <- get_graph(endpoint, method, edges, window=1, filter = '')

  # Replace foreign assets
  res[, token0.name := fixToken(token0.name)]
  res[, token1.name := fixToken(token1.name)]
  res[, id := fixToken(id)]

  d24 <- list()
  d7 <- list()
  for (i in 1:nrow(res)) {
    d24[[i]] <- as.numeric(res$dayData.[[i]]$volumeUSD[1]) / 1e18
    d7[[i]] <- sum(as.numeric(res$dayData.[[i]]$volumeUSD), na.rm = TRUE) / 1e18
  }
  res[, volumeUSD_24H := d24]
  res[, volumeUSD_7D := d7]
  res[, dayData. := NULL]
  res[, tvlUSD := as.numeric(tvlUSD) / 1e18]
  res[token0Amount > 0 | token1Amount > 0]

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getPoolStats_acala_dex <- function(network, window = 1) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "pools"
  edges <- "id
            token0 {decimals, name}
            token1 {decimals, name}
            token0Amount
            token1Amount
            totalTVL
            dayData(orderBy:TIMESTAMP_DESC,first:7) {
                nodes {
                    timestamp
                    totalTVL
                    dailyTradeVolumeUSD
                }
              }"
  res <- get_graph(endpoint, method, edges, window=1, filter = '')

  # Replace foreign assets
  res[, token0.name := fixToken(token0.name)]
  res[, token1.name := fixToken(token1.name)]
  res[, id := fixToken(id)]

  d24 <- list()
  d7 <- list()
  for (i in 1:nrow(res)) {
    d24[[i]] <- as.numeric(res$dayData.[[i]]$dailyTradeVolumeUSD[1]) / 1e18
    d7[[i]] <- sum(as.numeric(res$dayData.[[i]]$dailyTradeVolumeUSD), na.rm = TRUE) / 1e18
  }
  res[, volumeUSD_24H := d24]
  res[, volumeUSD_7D := d7]
  res[, totalTVL := as.numeric(totalTVL) / 1e18]
  res[, dayData. := NULL]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getPoolStats_acala_dex_testing <- function(network, window = 1) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "pools"
  edges <- "id
            token0 {decimals, name}
            token1 {decimals, name}
            token0Amount
            token1Amount
            token0TVL
            token1TVL
            totalTVL"
  res <- get_graph(endpoint, method, edges, window=1, filter = '')
  # res <- get_graph(endpoint, method, edges, window=1, filter = 'filter: {token0Id: {equalTo: "AUSD"}, token1Id: {equalTo: "LDOT"}}')

  # Replace foreign assets
  res[, token0.name := fixToken(token0.name)]
  res[, token1.name := fixToken(token1.name)]
  res[, id := fixToken(id)]

  res[, totalTVL := as.numeric(totalTVL) / 1e18]
  res[, token0TVL := as.numeric(token0TVL) / 1e18]
  res[, token1TVL := as.numeric(token1TVL) / 1e18]
  res[, testTVL := token0TVL + token1TVL]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getTokenDailyData_acala_dex <- function(network, window = 1) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "tokenDailyData"
  edges <- "tokenId amount tvl dailyTradeVolumeUSD dailyTxCount price timestamp"
  res <- get_graph(endpoint, method, edges, window=1, filter = '')

  # Replace foreign assets
  res[, tokenId := fixToken(tokenId)]

  res[, tvl := as.numeric(tvl) / 1e18]
  res[, dailyTradeVolumeUSD := as.numeric(dailyTradeVolumeUSD) / 1e18]
  res[, price := as.numeric(price) / 1e18]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getDailyPool_acala_dex <- function(network, window = 1) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "dailyPools"
  edges <- "poolId timestamp token0Id token1Id token0Amount token1Amount token0Price token1Price feeRateUSD token0TVL token1TVL totalTVL txCount"
  res <- get_graph(endpoint, method, edges, window=1, filter = '')

  # Replace foreign assets
  res[, token0Id := fixToken(token0Id)]
  res[, token1Id := fixToken(token1Id)]
  res[, poolId := fixToken(poolId)]

  res[, feeRateUSD := as.numeric(feeRateUSD) / 1e18]
  res[, token0Price := as.numeric(token0Price) / 1e18]
  res[, token1Price := as.numeric(token1Price) / 1e18]
  res[, token0TVL := as.numeric(token0TVL) / 1e18]
  res[, token1TVL := as.numeric(token1TVL) / 1e18]
  res[, totalTVL := as.numeric(totalTVL) / 1e18]

  res <- merge(res, tokens, by.x='token0Id', by.y='Token', allow.cartesian = TRUE)
  res[, adj0 := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res <- merge(res, tokens, by.x='token1Id', by.y='Token', allow.cartesian = TRUE)
  res[, adj1 := as.numeric(substr(as.character(1e20),1, as.numeric(decimals.y) + 1))]
  res[, token0Amount := as.numeric(token0Amount) / adj0]
  res[, token1Amount := as.numeric(token1Amount) / adj1]

  res[, decimals.x := NULL]
  res[, decimals.y := NULL]
  res[, Name.x := NULL]
  res[, Name.y := NULL]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getPoolDayData_acala <- function(network, window = 1) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }

  method <- "poolDayData"
  edges <- "poolId date token0Id token1Id token0Amount token1Amount tvlUSD"
  res <- get_graph(endpoint, method, edges, window=1, filter = '')

  # Replace foreign assets
  res[, token0Id := fixToken(token0Id)]
  res[, token1Id := fixToken(token1Id)]
  res[, poolId := fixToken(poolId)]

  res <- merge(res, tokens, by.x='token0Id', by.y='Token', allow.cartesian = TRUE)
  res[, adj0 := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res <- merge(res, tokens, by.x='token1Id', by.y='Token', allow.cartesian = TRUE)
  res[, adj1 := as.numeric(substr(as.character(1e20),1, as.numeric(decimals.y) + 1))]
  res[, token0Amount := as.numeric(token0Amount) / adj0]
  res[, token1Amount := as.numeric(token1Amount) / adj1]

  res[, tvlUSD := as.numeric(tvlUSD) / 1e18]
  res[, decimals.x := NULL]
  res[, decimals.y := NULL]
  res[, Name.x := NULL]
  res[, Name.y := NULL]
  res

}

