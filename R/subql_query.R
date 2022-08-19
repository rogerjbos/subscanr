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

stagingStr <- "__QWNhb"

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

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
myAmount <- function(path) {
  # path <- res$amounts
  path0 <- list()
  path1 <- list()
  path2 <- list()
  path3 <- list()
  path <- strsplit(path, ",")
  for (i in 1:length(path)) {
    path0[[i]] <- path[[i]][1]
    path1[[i]] <- path[[i]][2]
    path2[[i]] <- ifelse(path[[i]][3] == "", "", path[[i]][3])
    path3[[i]] <- ifelse(path[[i]][4] == "", "", path[[i]][4])
  }
  list(path0=path0, path1=path1, path2=path2, path3=path3)
}

# Helper function to concat
`%+%` <- function(a, b) paste0(a, b)

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
fixToken <- function(x, chain = "Karura") {

  fa0 <- ifelse(tolower(chain) == 'Karura', 'RMRK', 'GLMR')
  x <- stri_replace_all_regex(x, pattern=c('fa://0', 'ForeignAsset://0', 'fa%3A%2F%2F0', "\\{'ForeignAsset': 0\\}"), replacement=fa0, vectorize=FALSE)

  fa1 <- ifelse(tolower(chain) == 'Karura', 'ARIS', 'PARA')
  x <- stri_replace_all_regex(x, pattern=c('fa://1', 'ForeignAsset://1', 'fa%3A%2F%2F1', "\\{'ForeignAsset': 1\\}"), replacement=fa1, vectorize=FALSE)

  fa2 <- ifelse(tolower(chain) == 'Karura', 'QTZ', 'ASTR')
  x <- stri_replace_all_regex(x, pattern=c('fa://2', 'ForeignAsset://2', 'fa%3A%2F%2F2', "\\{'ForeignAsset': 2\\}"), replacement=fa2, vectorize=FALSE)

  fa3 <- ifelse(tolower(chain) == 'Karura', 'MOVR', 'iBTC')
  x <- stri_replace_all_regex(x, pattern=c('fa://3', 'ForeignAsset://3', 'fa%3A%2F%2F3', "\\{'ForeignAsset': 3\\}"), replacement=fa3, vectorize=FALSE)

  fa4 <- ifelse(tolower(chain) == 'Karura', 'HKO', 'INTR')
  x <- stri_replace_all_regex(x, pattern=c('fa://4', 'ForeignAsset://4', 'fa%3A%2F%2F4', "\\{'ForeignAsset': 4\\}"), replacement=fa4, vectorize=FALSE)

  fa5 <- ifelse(tolower(chain) == 'Karura', 'CSM', 'WBTC')
  x <- stri_replace_all_regex(x, pattern=c('fa://5', 'ForeignAsset://5', 'fa%3A%2F%2F5', "\\{'ForeignAsset': 5\\}"), replacement=fa5, vectorize=FALSE)

  fa6 <- ifelse(tolower(chain) == 'Karura', 'KICO', 'WETH')
  x <- stri_replace_all_regex(x, pattern=c('fa://6', 'ForeignAsset://6', 'fa%3A%2F%2F6', "\\{'ForeignAsset': 6\\}"), replacement=fa6, vectorize=FALSE)

  fa7 <- ifelse(tolower(chain) == 'Karura', 'USDT', 'EQ')
  x <- stri_replace_all_regex(x, pattern=c('fa://7', 'ForeignAsset://7', 'fa%3A%2F%2F7', "\\{'ForeignAsset': 7\\}"), replacement=fa7, vectorize=FALSE)

  fa8 <- ifelse(tolower(chain) == 'Karura', 'TEER', 'EQD')
  x <- stri_replace_all_regex(x, pattern=c('fa://8', 'ForeignAsset://8', 'fa%3A%2F%2F8', "\\{'ForeignAsset': 8\\}"), replacement=fa8, vectorize=FALSE)

  fa9 <- ifelse(tolower(chain) == 'Karura', 'NEER', 'PHA')
  x <- stri_replace_all_regex(x, pattern=c('fa://9', 'ForeignAsset://9', 'fa%3A%2F%2F9', "\\{'ForeignAsset': 9\\}"), replacement=fa9, vectorize=FALSE)

  fa10 <- ifelse(tolower(chain) == 'Karura', 'KMA', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://10', 'ForeignAsset://10', 'fa%3A%2F%2F10', "\\{'ForeignAsset': 10\\}"), replacement=fa10, vectorize=FALSE)

  fa11 <- ifelse(tolower(chain) == 'Karura', 'BSX', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://11', 'ForeignAsset://11', 'fa%3A%2F%2F11', "\\{'ForeignAsset': 11\\}"), replacement=fa11, vectorize=FALSE)

  fa12 <- ifelse(tolower(chain) == 'Karura', 'AIR', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://12', 'ForeignAsset://12', 'fa%3A%2F%2F12', "\\{'ForeignAsset': 12\\}"), replacement=fa12, vectorize=FALSE)

  fa13 <- ifelse(tolower(chain) == 'Karura', 'CRAB', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://13', 'ForeignAsset://13', 'fa%3A%2F%2F13', "\\{'ForeignAsset': 13\\}"), replacement=fa13, vectorize=FALSE)

  fa14 <- ifelse(tolower(chain) == 'Karura', 'GENS', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://14', 'ForeignAsset://14', 'fa%3A%2F%2F14', "\\{'ForeignAsset': 14\\}"), replacement=fa14, vectorize=FALSE)

  fa15 <- ifelse(tolower(chain) == 'Karura', 'EQD', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://15', 'ForeignAsset://15', 'fa%3A%2F%2F15', "\\{'ForeignAsset': 15\\}"), replacement=fa15, vectorize=FALSE)

  fa16 <- ifelse(tolower(chain) == 'Karura', 'TUR', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://16', 'ForeignAsset://16', 'fa%3A%2F%2F16', "\\{'ForeignAsset': 16\\}"), replacement=fa16, vectorize=FALSE)

  fa17 <- ifelse(tolower(chain) == 'Karura', 'PCHU', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://17', 'ForeignAsset://17', 'fa%3A%2F%2F17', "\\{'ForeignAsset': 17\\}"), replacement=fa17, vectorize=FALSE)

  fa18 <- ifelse(tolower(chain) == 'Karura', 'SDN', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://18', 'ForeignAsset://18', 'fa%3A%2F%2F18', "\\{'ForeignAsset': 18\\}"), replacement=fa18, vectorize=FALSE)

  fa19 <- ifelse(tolower(chain) == 'Karura', 'LT', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://19', 'ForeignAsset://19', 'fa%3A%2F%2F19', "\\{'ForeignAsset': 19\\}"), replacement=fa19, vectorize=FALSE)

  fa20 <- ifelse(tolower(chain) == 'Karura', 'LIT', '')
  x <- stri_replace_all_regex(x, pattern=c('fa://20', 'ForeignAsset://20', 'fa%3A%2F%2F20', "\\{'ForeignAsset': 20\\}"), replacement=fa20, vectorize=FALSE)

  lc13 <- 'LCDOT'
  x <- stri_replace_all_regex(x, pattern=c('lc://13', 'LiquidCrowdloan://13', 'lc%3A%2F%2F13', "\\{'LiquidCrowdloan': 13\\}"), replacement=lc13, vectorize=FALSE)

  sa0 <- ifelse(tolower(chain) == 'Karura', 'taiKSM', 'tDOT')
  x <- stri_replace_all_regex(x, pattern=c('sa://0', 'StableAssetPoolToken://0', 'sa%3A%2F%2S0', "\\{'StableAssetPoolToken': 0\\}"), replacement=sa0, vectorize=FALSE)

  sa1 <- ifelse(tolower(chain) == 'Karura', '3USD', '')
  x <- stri_replace_all_regex(x, pattern=c('sa://1', 'StableAssetPoolToken://1', 'sa%3A%2F%2S1', "\\{'StableAssetPoolToken': 1\\}"), replacement=sa1, vectorize=FALSE)

  x <- gsub("{'Token': 'BNC'}", 'BNC', x, perl = TRUE)
  x <- gsub("{'Token': 'KAR'}", 'KAR', x, perl = TRUE)
  x <- gsub("{'Token': 'KBTC'}", 'KBTC', x, perl = TRUE)
  x <- gsub("{'Token': 'KINT'}", 'KINT', x, perl = TRUE)
  x <- gsub("{'Token': 'KSM'}", 'KSM', x, perl = TRUE)
  x <- gsub("{'Token': 'VSKSM'}", 'VSKSM', x, perl = TRUE)
  x <- gsub("{'Token': 'KUSD'}", 'KUSD', x, perl = TRUE)
  x <- gsub("{'Token': 'LKSM'}", 'LKSM', x, perl = TRUE)
  x <- gsub("{'Token': 'PHA'}", 'PHA', x, perl = TRUE)
  x <- gsub("{'Token': 'TAI'}", 'TAI', x, perl = TRUE)
  x <- gsub("{'Token': 'ACA'}", 'ACA', x, perl = TRUE)
  x <- gsub("{'Token': 'AUSD'}", 'AUSD', x, perl = TRUE)
  x <- gsub("{'Token': 'DOT'}", 'DOT', x, perl = TRUE)
  x <- gsub("{'Token': 'LDOT'}", 'LDOT', x, perl = TRUE)
  x <- gsub("{'DexShare' ", 'lp:', x, perl = TRUE)
  x <- gsub("}", '', x, perl = TRUE)
}



#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
tokens <- as.data.table(rbind(c("3USD", "Taiga 3USD", 12),
                              c("ACA", "aca-token", 12),
                              c("AIR","altair", 18),
                              c("ARIS","polaris-dao", 8),
                              c("ARIS2","polaris-dao", 8),
                              c("AUSD","acala-dollar", 12),
                              c("ASTR","astar", 18),
                              c("BNC","bifrost-native-coin", 12),
                              c("BSX","basilisk", 12),
                              c("CASH","compound-cash", 8),
                              c("CRAB","darwinia-crab-network", 18),
                              c("CSM","crust-storage-market", 12),
                              c("DAI","dai-stablecoin-portal", 18),
                              c("DOT","polkadot", 10),
                              c("DOT (on Homa)","staked polkadot", 10),
                              c("EQ","equilibrium", 9),
                              c("EQD","equilibrium-usd", 9),
                              c("GENS","genshiro", 9),
                              c("GLMR","moonbeam", 18),
                              c("HKO","heiko", 12),
                              c("KAR","karura", 12),
                              c("KBTC","kintsugi-btc", 8),
                              c("KICO","kico", 14),
                              c("KINT","kintsugi", 12),
                              c("KMA","calamri", 12),
                              c("KSM","kusama", 12),
                              c("KSM (on Homa)","staked-kusama", 12),
                              c("KUSD","karura-dollar", 12),
                              c("iBTC","inter-btc", 8),
                              c("INTR","interlay", 10),
                              c("LCDOT","liquid-crowdloan-dot", 10),
                              c("LDOT","liquid-dot", 10),
                              c("LKSM","liquid-ksm", 12),
                              c("LT","listen", 12),
                              c("LIT","litmus", 12),
                              c("MOVR","moonriver", 18),
                              c("NEER","metaverse-network-pioneer", 18),
                              c("PARA","parallel", 12),
                              c("PCHU","pichiu-portal", 18),
                              c("PHA","pha", 12),
                              c("QTZ","quartz", 18),
                              c("RMRK","rmrk", 10),
                              c("RENBTC","renbtc", 8),
                              c("TAI","tai", 12),
                              c("SDN","shiden", 18),
                              c("tDOT","tapio-DOT", 10),
                              c("TAP","tapio", 12),
                              c("TUR","turing", 10),
                              c("tDOT","tapio-dot", 10),
                              c("taiKSM","tai-ksm", 12),
                              c("TEER","integritee-trusted-execution-environment", 12),
                              c("USDC","usd-coin", 6),
                              c("USDT","tether-usd", 6),
                              c("USDCet","usd-coin-portal-from-ethereum", 6),
                              c("USDT","tether", 6),
                              c("WAUSD","acala-dollar-portal", 12),
                              c("WETH","wrapped-ether", 18),
                              c("WBTC","wrapped-bitcoin", 8),
                              c("VSKSM","bifrost-voucher-slot-ksm", 12)))
setnames(tokens, c("Token","Name","decimals"))
try(tokens[, divisor := (as.numeric(substr(as.character(1e20), 1, as.numeric(decimals) + 1))), by = Token], silent = TRUE)

# Query function for very simple queries
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_query <- function(url, query) {

  # url <- 'https://api.subquery.network/sq/AcalaNetwork/karura-loan'
  # query <- 'query { collateralParamsHistories { nodes { collateral {id} maximumTotalDebitValue interestRatePerSec liquidationRatio liquidationPenalty requiredCollateralRatio startAtBlockId endAtBlockId startAt endAt } } }'

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
  res <- rbindlist(resList, use.names = TRUE, fill = TRUE)
  setnames(res, old = names(res), new = gsub("node.", "", names(res)))

  res

}



#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getDailyPools_acala_dex <- function(network, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "dailyPools"
  edges <- "timestamp token0 {id} token1 {id} feeRateUSD dailyTradeVolumeUSD totalTVL txCount updateAtBlock {id}"
  res <- get_graph(endpoint, method, edges, window)

  res[, Date := as.Date(timestamp)]
  setorder(res, Date)
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
getLiquidity_acala <- function(endpoint, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

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
getLoansCollateralParams_acala_loan <- function(network, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

    method <- "collateralParams"
  edges <- "collateral {id} maximumTotalDebitValue interestRatePerSec liquidationRatio
              liquidationPenalty requiredCollateralRatio updateAt updateAtBlockId"
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
getLoansCollateralParamsHistory_acala_loan <- function(network, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "collateralParamsHistories"
  edges <- "collateral {id} maximumTotalDebitValue interestRatePerSec liquidationRatio
              liquidationPenalty requiredCollateralRatio
              startAtBlockId endAtBlockId startAt endAt"
  res <- get_graph(endpoint, method, edges, filter = "", window=1)

  # res[, startAtBlockId := as.numeric(startAtBlockId)]
  # setorder(res, "startAtBlockId")
  # res[collateral.id=="KSM", 1:7]
  # sort(unique(res$collateral.id))

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
getLoansDailyPositions_acala_loan <- function(network, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "dailyPositions"
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
getLoansPositions_acala_loan <- function(network, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  # filter <- 'filter: {collateralId: {in: ["ACA","DOT","LDOT","KSM","LKSM"]}} '; endpage <- 2
  method <- "positions"
  edges <- "owner {id} collateral {id} txCount depositAmount debitAmount updateAt updateAtBlock {id}"
  res <- get_graph(endpoint, method, edges, window, filter = "")

  res[, collateral.id := fixToken(collateral.id)]
  res <- merge(res, tokens, by.x='collateral.id', by.y='Token')
  res[, Name := NULL]

  # res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, depositAmount := as.numeric(depositAmount) / 10**as.numeric(decimals)]
  res[, debitAmount := as.numeric(debitAmount) / 10**as.numeric(decimals)]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLoansCollateral_acala_loan <- function(network, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "collaterals"
  edges <- "id name decimals depositAmount debitAmount txCount"
  res <- get_graph(endpoint, method, edges, window, filter = "")

  stopifnot(nrow(res) > 0)
  res[, id := fixToken(id)]
  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, depositAmount := as.numeric(depositAmount) / adj]
  res[, debitAmount := as.numeric(debitAmount) / adj]
  res

}



#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLoansUpdatePosition_acala_loan <- function(network, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "updatePositions"
  edges <- "id owner {id} collateral {id} collateralAdjustment debitAdjustment collateralAdjustmentUSD debitAdjustmentUSD price debitExchangeRate isDerived block {id} extrinsic {id} timestamp"
  res <- get_graph(endpoint, method, edges, window, filter = "timestamp")

  stopifnot(nrow(res) > 0)
  res[, id := fixToken(id)]
  res <- merge(res, tokens, by.x = "collateral.id", by.y = "Token")
  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, collateralAdjustment := as.numeric(collateralAdjustment) / adj]
  res[, debitAdjustment := as.numeric(debitAdjustment) / adj]
  res[, collateralAdjustmentUSD := as.numeric(collateralAdjustmentUSD) / 10**18]
  res[, debitAdjustmentUSD := as.numeric(debitAdjustmentUSD) / 10**18]
  res[, price := as.numeric(price) / 10**18]

  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getLoansDailyCollateral_acala_loan <- function(network, window, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "dailyCollaterals"
  edges <- "collateral {id} depositAmount debitAmount depositVolumeUSD debitVolumeUSD
            depositChangedUSD debitChangedUSD debitExchangeRate timestamp txCount"
  res <- get_graph(endpoint, method, edges, window)

  stopifnot(nrow(res) > 0)
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
getSwaps_acala_dex <- function(network, window, block = NULL, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "swaps"
  edges <- "id address {id} pool {id}  token0 {id} token1 {id} token0InAmount token1OutAmount tradePath price0 price1
            amounts block {id} extrinsic {id} timestamp"
  if (!is.null(block)) {
    filter <- 'filter: {blockId: {equalTo: "' %+% block %+% '"}} '
    res <- get_graph(endpoint, method, edges, window, filter)
  } else {
    res <- get_graph(endpoint, method, edges, window)
  }

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

  # Create pair0, pair1, etc.
  z <- myPath(res$tradePath)
  res[, path0 := z$path0][, path1 := z$path1][, path2 := z$path2]
  # Create token0, token1, etc.
  a1 <- myAmount(res$tradePath)
  res[, tokenPath0 := a1$path0][, tokenPath1 := a1$path1][, tokenPath2 := a1$path2][, tokenPath3 := a1$path3]
  res[, tokenPath0 := fixToken(tokenPath0)]
  res[, tokenPath1 := fixToken(tokenPath1)]
  res[, tokenPath2 := fixToken(tokenPath2)]
  res[, tokenPath3 := fixToken(tokenPath3)]

  # Create amount0, amount1, etc.
  a2 <- myAmount(res$amounts)
  res[, amount0 := a2$path0][, amount1 := a2$path1][, amount2 := a2$path2][, amount3 := a2$path3]

  # res[, exclude := token0 == token1]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getSwaps_acala <- function(network, window, block = NULL, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

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
getLiquidateUnsafeCDP_acala_loan <- function(network, window, staging = FALSE) {

  # staging <- TRUE
  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-loans"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-loan"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "liquidUnsaves"
  edges <- "sender {id} owner {id} collateral {id} collateralAmount collateralVolumeUSD
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
getAccounts_acala <- function(network, window, filter = '', endpage = 2000, staging = FALSE) {

  # network="acala"; window = 1; filter = ''; endpage = 2

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "accounts"
  edges <- "id txCount createAtBlockId"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getRewards_acala_incentives <- function(network, window, filter = '', endpage = 2000, staging = FALSE) {

  # network="acala"; window = 1; filter = ''; endpage = 2

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-incentives"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-incentives"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "claimRewards"
  edges <- "id addressId tokenId pool actualAmount deductionAmount blockId extrinsicId timestamp"
  filter <- ' filter: {blockId: { lessThan: "1639493", greaterThan: "1638215" }}'
  res <- get_graph(endpoint, method, edges, window, filter, endpage)
  res[, tokenId := fixToken(tokenId)]
  res[, pool := fixToken(pool)]

  res <- merge(res, tokens, by.x = "tokenId", by.y="Token")
  # res[, actualAmount := as.numeric(actualAmount) / 10**as.numeric(decimals)]
  # res[, deductionAmount := as.numeric(deductionAmount) / 10**as.numeric(decimals)]
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
getDailyAccountBalance_acala_token <- function(network, window, filter = '', endpage = 2000, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-tokens-ipfs"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-tokens-ipfs"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "dailyAccountBalances"
  edges <- "accountId tokenId timestamp total free reserved frozen updateAtBlock"
  # filter <- ' filter: {accountId: {equalTo: "23M5ttkmR6Kco5p3LFGKMpMv4zvLkKdUQWW1wGGoV8zDX3am"}}'
  res <- get_graph(endpoint, method, edges, window, filter, endpage)
  # unique(res$tokenId)

  res[, tokenId := subscanr::fixToken(tokenId)]
  res <- merge(res, tokens, by.x='tokenId', by.y='Token')
  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, total := as.numeric(total) / adj]
  res[, free := as.numeric(free) / adj]
  res[, reserved := as.numeric(reserved) / adj]
  res[, frozen := as.numeric(frozen) / adj]
  res[, Name := NULL]
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getAccountBalance_acala_token <- function(network, window, filter = '', endpage = 2000, staging = FALSE) {

  # query {\n        accountBalances (filter: {tokenId: {in: [\"DOT\",\"LDOT\"]}, total: {greaterThan: \"0\"}}  first:100) {\n          totalCount\n          edges {\n            node { accountId tokenId total}\n            cursor\n          }\n          pageInfo {\n            endCursor\n            hasNextPage\n          }\n        }\n      }\n    }"
  # network="acala"; window = 1; filter = 'filter: {tokenId: {in: ["LDOT"]}, total: {greaterThan: "0"}} '; endpage = 2e9
  # network="karura"; window = 1; filter = 'filter: {tokenId: {in: ["LKSM"]}, total: {greaterThan: "0"}} '; endpage = 2e9
  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-tokens-ipfs"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-tokens-ipfs"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "accountBalances"
  edges <- "accountId tokenId total free reserved frozen"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  res[, tokenId := subscanr::fixToken(tokenId)]
  res <- merge(res, tokens, by.x='tokenId', by.y='Token')
  res[, adj := as.numeric(substr(as.character(1e20),1, as.numeric(decimals) + 1))]
  res[, total := as.numeric(total) / adj]
  res[, free := as.numeric(free) / adj]
  res[, reserved := as.numeric(reserved) / adj]
  res[, frozen := as.numeric(frozen) / adj]
  res[, Name := NULL]
  res

}


#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getMint_acala_homa <- function(network, window, filter = '', endpage = 1000, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-homa"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-homa"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "mints"
  edges <- "address {id} type amountStaked amountMinted stakingCurrencyAmount liquidAmountReceived liquidAmountAddedToVoid block {id} extrinsic {id} timestamp"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  setorder(res, timestamp)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getRequestedRedeem_acala_homa <- function(network, window, filter = '', endpage = 1000, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-homa"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-homa"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "requestedRedeems"
  edges <- "address {id} amount allowFastMatch block {id} extrinsic {id} timestamp"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  setorder(res, timestamp)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getRedeemRequestCancelled_acala_homa <- function(network, window, filter = '', endpage = 1000, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-homa"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-homa"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "redeemRequestCancelleds"
  edges <- "address {id} amount block {id} extrinsic {id} timestamp"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  setorder(res, timestamp)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getRedeemByUnbond_acala_homa <- function(network, window, filter = '', endpage = 100, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-homa"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-homa"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "redeemedByUnbonds"
  edges <- "address {id} eraIndexWhenUnbond liquidAmount unbondingStakingAmount block {id} extrinsic {id} timestamp"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  setorder(res, timestamp)
  res

}


#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getRedeemByFastMatch_acala_homa <- function(network, window, filter = '', endpage = 100, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-homa"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-homa"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "redeemedByFastMatches"
  edges <- "address {id} matchedLiquidAmount feeInLiquid redeemedStakingAmount block {id} extrinsic {id} timestamp"
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  setorder(res, timestamp)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getRewards_acala <- function(network, window, filter = '', endpage = 2000, staging = FALSE) {

  # network="acala"; window = 1; filter = ''; endpage = 2

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-subql"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-subql"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "events"
  edges <- "id data"
  filter <- ' filter: {blockNumber: { lessThan: "1639493", greaterThan: "1638215" } section: { equalTo: "incentives" } method: { equalTo: "ClaimRewards" }}'
  res <- get_graph(endpoint, method, edges, window, filter, endpage)

  data_list <- list()
  for (ii in 1:nrow(res)) {
    data_list[[ii]] <- res[ii]$data[[1]]$value %>% t %>% as.data.table
  }
  data <- rbindlist(data_list) %>%
    setnames(c("addressId","poolId","tokenId","reward","unknown"))
  res[, data := NULL]
  res <- cbind(res, data)
  res

}

#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
getPoolStats_acala <- function(network, window = 1, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

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
getPoolStats_acala_dex <- function(network, window = 1, filter = '', staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

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
  res <- get_graph(endpoint, method, edges, window=1, filter = filter)

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
getPoolStats_acala_dex_testing <- function(network, window = 1, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

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
getTokenDailyData_acala_dex <- function(network, window = 1, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

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
getDailyPool_acala_dex <- function(network, window = 1, filter = '', staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala-dex"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura-dex"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "dailyPools"
  edges <- "poolId timestamp token0Id token1Id token0Amount token1Amount token0Price token1Price feeRateUSD token0TVL token1TVL totalTVL txCount"
  filter = ''
  res <- get_graph(endpoint, method, edges, window=1, filter = filter)

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
getPoolDayData_acala <- function(network, window = 1, staging = FALSE) {

  if (tolower(network) == 'acala') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/acala"
  } else if (tolower(network) == 'karura') {
    endpoint <- "https://api.subquery.network/sq/AcalaNetwork/karura"
  } else {
    stop("Network not found; must be one of 'acala' or 'karura'")
  }
  if (staging) endpoint <- endpoint %+% stagingStr

  method <- "poolDayData"
  edges <- "poolId date token0Id token1Id token0Amount token1Amount tvlUSD token0Close"
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
  res[, token0Close := as.numeric(token0Close) / 1e18]
  res[, decimals.x := NULL]
  res[, decimals.y := NULL]
  res[, Name.x := NULL]
  res[, Name.y := NULL]
  res

}

