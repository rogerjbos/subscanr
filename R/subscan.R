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
                   "Acala","acala.api.subscan.io",
                   "Alephzero", "alephzero.api.subscan.io",
                   "Astar","astar.api.subscan.io",
                   "Bifrost Asgard","bifrost.api.subscan.io",
                   "Calamari", "calamari.api.subscan.io",
                   "Centrifuge","centrifuge.api.subscan.io",
                   "ChainX","chainx.api.subscan.io",
                   "Clover","clover.api.subscan.io",
                   "Clover Parachain","clv.api.subscan.io",
                   "Clover Testnet","clover-testnet.api.subscan.io",
                   "Crust Maxwell","maxwell.api.subscan.io",
                   "Crust Shadow","shadow.api.subscan.io",
                   "Darwinia Crab","crab.api.subscan.io",
                   "DataHighway","datahighway.api.subscan.io",
                   "DataHighway","Harbour	datahighway-harbour.api.subscan.io",
                   "DeepBrain Chain","dbc.api.subscan.io",
                   "Dock","dock.api.subscan.io",
                   "Dolphin","dolphin.api.subscan.io",
                   "Edgeware","edgeware.api.subscan.io",
                   "Equilibrium","equilibrium.api.subscan.io",
                   "Gateway","gateway-testnet.api.subscan.io",
                   "Karura","karura.api.subscan.io",
                   "Kulupu","kulupu.api.subscan.io",
                   "Khala","khala.api.subscan.io",
                   "KILT Peregrine","kilt-testnet.api.subscan.io",
                   "KILT Spiritnet	spiritnet.api.subscan.io",
                   "Laminar TC2","laminar-testnet.api.subscan.io",
                   "Litentry","litentry.api.subscan.io",
                   "Manta","manta-testnet.api.subscan.io",
                   "Moonbase","moonbase.api.subscan.io",
                   "Moonbeam","moonbeam.api.subscan.io",
                   "Moonriver","moonriver.api.subscan.io",
                   "Pangolin","pangolin.api.subscan.io",
                   "Pangoro","pangoro.api.subscan.io",
                   "Parallel","parallel.api.subscan.io",
                   "Parallel Heiko","parallel-heiko.api.subscan.io",
                   "Phala Rorschach","phala.api.subscan.io",
                   "Picasso","picasso.api.subscan.io",
                   "Polkadex","polkadex.api.subscan.io",
                   "Polymesh","polymesh.api.subscan.io",
                   "Polymesh Testnet","polymesh-testnet.api.subscan.io",
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



tokens <- rbind(c("ACA", "Acala", 12),
                c("AUSD","Acala Dollar", 12),
                c("DOT","Polkadot", 10),
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
                c("KBTC","Kintsugi Wrapped BTC", 8)) %>%
  as.data.table %>%
  setnames(c("Token","Name","decimals"))



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
get_subscan_events <- function(nobs = 100, network = 'Acala', start_page = 1, module = '', call = '', extract = TRUE) {

  # nobs = 100; network = 'Astar'; module = ''; call = ''; page = 1
  # nobs = 5; network = 'Acala'; module = 'dex'; call = 'Swap'; page = 1; start_page = 1

  api_host <- endpoints[network_name == network, api_host]
  api_call <- '/api/scan/events'
  baseurl <- paste0('https://', api_host, api_call)
  fname <- network %+% "_events.csv"

  # each `page` pulls in 100 rows, so calculate how many pages you need to pull
  last_page <- ceiling(max(1, (nobs /100))) + (start_page - 1)

  page_list <-list()
  params_list <-list()
  for (page in start_page:last_page) {

    body <- '{"row": ' %+% min(100, nobs) %+% ',"page": ' %+% page %+% ',"module": "' %+% module %+% '","call": "' %+% call %+% '"}'
    # body <- '{"row": ' %+% min(100, nobs) %+% ',"page": ' %+% page %+% ',"module": "' %+% module %+% '"}'
    if (page %% 2 == 0) Sys.sleep(1)
    r <- POST(baseurl, body = body,
              add_headers(api_header, 'Content-Type=application/json'))
    stop_for_status(r)
    tmp <- content(r, as="text", encoding="UTF-8") %>%
      fromJSON(flatten=TRUE)

    core_data <- tmp$data$events %>%
      as.data.table
    params <- core_data$params

    if (nrow(core_data) == 0) {
      page <- last_page
      # break
    } else {
      core_data <- core_data[, params := NULL]

      print(nrow(core_data) %+% " rows for page " %+% page %+% "/" %+% last_page %+% " " %+% tmp$message %+% " at " %+% Sys.time())
      page_list[[page]] <- core_data
      params_list[[page]] <- params

    }



  }
  core_data <- rbindlist(page_list)
  params <- do.call("c", params_list)


  if (extract) return(extract_events(core_data, params))

  list(core_data = core_data, params = params)

}

extract_events <- function(core_data, params) {
# core_data <- tmp$core_data; params <- tmp$params

  if (nrow(core_data) > 0) {

    # add a human-readable date
    core_data[, time := as.POSIXct(block_timestamp, origin = "1970-01-01", tz = 'UTC')]

    if (any(core_data$module_id %in% c('incentives'))) {
      incentives_DepositDexShare_list <- list()
      incentives_WithdrawDexShare_list <- list()
      incentives_ClaimRewards_list <- list()
    }
    if (any(core_data$module_id %in% c('dex','zenlinkprotocol'))) {
      dex_Swap_list <- list()
      dex_AddLiquidity_list <- list()
      dex_RemoveLiquidity_list <- list()
    }
    if (any(core_data$module_id %in% c('balances'))) {
      balances_Transfer_list <- list()
      balances_Deposit_list <- list()
      balances_Withdraw_list <- list()
    }
    if (any(core_data$module_id %in% c('loans'))) {
      loans_ConfiscateCollateralAndDebit_list <- list()
      loans_PositionUpdated_list <- list()
    }
    if (any(core_data$module_id %in% c('currencies'))) {
      currencies_Transferred_list <- list()
      currencies_Deposited_list <- list()
      currencies_Withdrawn_list <- list()
    }
    if (any(core_data$module_id %in% c('treasury'))) {
      treasury_Deposited_list <- list()
    }
    if (any(core_data$module_id %in% c('auctionmanager'))) {
      auctionmanager_DEXTakeCollateralAuction_list <- list()
      auctionmanager_CollateralAuctionDealt_list <- list()
    }
    if (any(core_data$module_id %in% c('cdpengine'))) {
      cdpengine_LiquidateUnsafeCDP_list <- list()
    }

    for (i in 1:length(params)) {
        ti <- fromJSON(params[[i]], flatten=TRUE)
        ti

        if (core_data[i, module_id] == "cdpengine") {
          if (core_data[i, event_id] == "LiquidateUnsafeCDP") {
            out <- data.table(t(ti$value))
            out[,1] <- ifelse(names(ti$value[[1]])=="Token", ti$value[[1]]$Token, names(ti$value[[1]]) %+% "://" %+%  ti$value[[1]][[1]])
            out[,5] <- names(ti$value[[5]]) %+% ":" %+%  ti$value[[5]][[1]]
            # if (network == 'Acala') {
            #   names(out) <- c("CurrencyId","AuctionId","CollateralAmount","BadDebtValue","TargetAmount")
            # } else {
            names(out) <- c("CurrencyId","AuctionId","CollateralAmount","BadDebtValue","LiquidationStrategy")
            # }
            cdpengine_LiquidateUnsafeCDP_list[[i]] <- data.table(core_data[i], out)
          }
        } else if (core_data[i, module_id] == "auctionmanager") {
          if (core_data[i, event_id] == "DEXTakeCollateralAuction") {
            out <- data.table(t(ti$value))
            out[,2] <- ifelse(names(ti$value[[2]])=="Token", ti$value[[2]]$Token, names(ti$value[[2]]) %+% "://" %+%  ti$value[[2]][[1]])
            if (ncol(out) == 5) {
              names(out) <- c("AuctionId","CurrencyId","CollateralAmount","SupplyCollateralAmount","TargetStableAmount")
            } else {
              names(out) <- c("AuctionId","CurrencyId","SupplyCollateralAmount","TargetStableAmount")
            }
            auctionmanager_DEXTakeCollateralAuction_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "CollateralAuctionDealt") {
            out <- data.table(t(ti$value))
            out[,2] <- ifelse(names(ti$value[[2]])=="Token", ti$value[[2]]$Token, names(ti$value[[2]]) %+% "://" %+%  ti$value[[2]][[1]])
            names(out) <- c("AuctionId","CurrencyId","CollateralAmount","WinnerId","PaymentAmount")
            auctionmanager_CollateralAuctionDealt_list[[i]] <- data.table(core_data[i], out)
          }
        } else if (core_data[i, module_id] == "treasury") {
            out <- data.table("BalanceOf" = ti$value[[1]])
            treasury_Deposited_list[[i]] <- data.table(core_data[i], out)
        } else if (core_data[i, module_id] == "incentives") {
          if (core_data[i, event_id] == "DepositDexShare") {
            out <- data.table("AccountId"=ti$value[[1]],
                              "token0Id"=ti$value[[2]][[1]][[1]][[1]],
                              "token1Id"=ti$value[[2]][[1]][[2]][[1]],
                              "Amount"=ti$value[[3]])

            incentives_DepositDexShare_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "WithdrawDexShare") {
            out <- data.table("AccountId"=ti$value[[1]],
                              "token0Id"=ti$value[[2]][[1]][[1]][[1]],
                              "token1Id"=ti$value[[2]][[1]][[2]][[1]],
                              "Amount"=ti$value[[3]])

            incentives_WithdrawDexShare_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "ClaimRewards") {
            id0 <- ti$value[[2]][[1]][[1]][[1]]
            if (length(ti$value[[2]][[1]]) == 1) {
              id1 <- ""
            } else {
              id1 <- ti$value[[2]][[1]][[1]][[2]]
            }
            out <- data.table("AccountId"=ti$value[[1]],
                              "type"= names(ti$value[[2]]),
                              "token0Id"=id0,
                              "token1Id"=id1,
                              "rewardToken"=ti$value[[3]][[1]],
                              "amount0"=ti$value[[4]],
                              "amount1"=ti$value[[5]])

            incentives_ClaimRewards_list[[i]] <- data.table(core_data[i], out)
          }

        } else if (core_data[i, module_id] == "currencies") {
          if (core_data[i, event_id] == "Transferred") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            test <- try(ti$value[[1]][[1]][[2]], silent=TRUE)
            if (inherits(test, "try-error")) {
              token0Id <- ti$value[[1]][[1]]
              token1Id <- ""
            } else {
              token0Id <- ti$value[[1]][[1]][[1]][[1]]
              token1Id <- ti$value[[1]][[1]][[2]][[1]]
            }
            out <- data.table(token0Id, token1Id, out[, -1])
            setnames(out, c("token0Id","token1Id","Account0Id","Account1Id","BalanceOf"))
            currencies_Transferred_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "Deposited") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            if (length(ti$value[[1]][[1]]) == 4) {
              token0Id <- ti$value[[1]][[1]][[1]]
              token1Id <- ti$value[[1]][[1]][[3]]
            } else if (length(ti$value[[1]][[1]]) == 2) {
              token0Id <- ti$value[[1]][[1]][[1]]
              token1Id <- ti$value[[1]][[1]][[2]]
            } else {
              token0Id <- ti$value[[1]][[1]][[1]]
              token1Id <- ""
            }
            out <- data.table(token0Id, token1Id, out[, -1])
            currencies_Deposited_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "Withdrawn") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            if (length(ti$value[[1]][[1]]) == 4) {
              token0Id <- ti$value[[1]][[1]][[1]]
              token1Id <- ti$value[[1]][[1]][[3]]
            } else if (length(ti$value[[1]][[1]]) == 2) {
              token0Id <- ti$value[[1]][[1]][[1]]
              token1Id <- ti$value[[1]][[1]][[2]]
            } else {
              token0Id <- ti$value[[1]][[1]][[1]]
              token1Id <- ""
            }
            out <- data.table(token0Id, token1Id, out[, -1])
            currencies_Withdrawn_list[[i]] <- data.table(core_data[i], out)
          }

        } else if (core_data[i, module_id] == "loans") {
          if (core_data[i, event_id] == "ConfiscateCollateralAndDebit") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            out[,2] <- ifelse(names(ti$value[[2]])=="Token", ti$value[[2]]$Token, names(ti$value[[2]]) %+% "://" %+%  ti$value[[2]][[1]])
            setnames(out, c("AccountId", "CurrencyId", "confiscatedCollateralAmount", "deductDebitAmount"))
            loans_ConfiscateCollateralAndDebit_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "PositionUpdated") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            out[,2] <- ifelse(names(ti$value[[2]])=="Token", ti$value[[2]]$Token, names(ti$value[[2]]) %+% "://" %+%  ti$value[[2]][[1]])
            setnames(out, c("AccountId", "CurrencyId", "collateralAdjustment", "debitAdjustment"))
            loans_PositionUpdated_list[[i]] <- data.table(core_data[i], out)
          }
        } else if (core_data[i, module_id] == "balances") {

          if (core_data[i, event_id] == "Transfer") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            balances_Transfer_list[[i]] <- data.table(core_data[i], out)

          } else if (core_data[i, event_id] == "Deposit") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            balances_Deposit_list[[i]] <- data.table(core_data[i], out)

          } else if (core_data[i, event_id] == "Withdraw") {
            out <- data.table(t(ti$value)) %>% setnames(ti$type_name)
            balances_Withdraw_list[[i]] <- data.table(core_data[i], out)
          }
        } else if (core_data[i, module_id] == "dex") {

          if (core_data[i, event_id] == "AddLiquidity") {
            out <- data.table(t(ti$value))
            out[,2] <- ifelse(names(ti$value[[2]])=="Token", ti$value[[2]]$Token, names(ti$value[[2]]) %+% "://" %+%  ti$value[[2]][[1]])
            out[,4] <- ifelse(names(ti$value[[4]])=="Token", ti$value[[4]]$Token, names(ti$value[[4]]) %+% "://" %+%  ti$value[[4]][[1]])
            setnames(out, c("AccountId","token0Id","token0Amount","token1Id","token1Amount","balance"))
            dex_AddLiquidity_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "RemoveLiquidity") {
            out <- data.table(t(ti$value))
            out[,2] <- ifelse(names(ti$value[[2]])=="Token", ti$value[[2]]$Token, names(ti$value[[2]]) %+% "://" %+%  ti$value[[2]][[1]])
            out[,4] <- ifelse(names(ti$value[[4]])=="Token", ti$value[[4]]$Token, names(ti$value[[4]]) %+% "://" %+%  ti$value[[4]][[1]])
            setnames(out, c("AccountId","token0Id","token0Amount","token1Id","token1Amount","balance"))
            dex_RemoveLiquidity_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "Swap") {
            n <- names(ti$value[[2]])
            if (length(n) == 1) {
              ids <- ti$value[[2]][1]
            } else {
              ids <- ti$value[[2]]['Token']
              alt_name <- n[!n %in% "Token"]
              alt <- coredata(ti$value[[2]][alt_name])
              ids[is.na(ids)] <- alt_name %+% "://" %+%  coredata(alt)[[1]][is.na(ids)]
            }
            amts <- ti$value[[3]]

            out <- data.table(account = ti$value[[1]], NA, NA, NA, NA, NA, NA, NA, NA)
            for (j in 1:length(ids$Token)) {
              out[, j + 1] <- ids$Token[j]
              out[, j + 5] <- amts[j]
            }

            names(out) <- c("account","token0","token1","token2","token3","amount0","amount1","amount2","amount3")

            out[, token0 := fixToken(token0)]
            out[, token1 := fixToken(token1)]
            out[, token2 := fixToken(token2)]
            out[, token3 := fixToken(token3)]
            # Normalize pairs
            out[, pair0 := paste0(token0 %+% ":" %+% token1)]
            out[token1 < token0, pair0 := paste0(token1 %+% ":" %+% token0)]
            out[, pair1 := paste0(token1 %+% ":" %+% token2)]
            out[token2 < token1, pair1 := paste0(token2 %+% ":" %+% token1)]
            out[, pair2 := paste0(token2 %+% ":" %+% token3)]
            out[token3 < token2, pair2 := paste0(token3 %+% ":" %+% token2)]

            dex_Swap_list[[i]] <- data.table(core_data[i], out)
          }
        } else if (core_data[i, module_id] == "zenlinkprotocol") {

          if (core_data[i, event_id] == "LiquidityAdded") {
            out <- data.table(t(ti$value))
            out[,2] <- ti$value[[2]][[1]] %+% "," %+% ti$value[[2]][[2]] %+% "," %+% ti$value[[2]][[3]]
            out[,3] <- ti$value[[3]][[1]] %+% "," %+% ti$value[[3]][[2]] %+% "," %+% ti$value[[3]][[3]]
            setnames(out, c("accountId","token0Id","token1Id","token0Amount","token1Amount","burnAmount"))
            dex_AddLiquidity_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "LiquidityRemoved") {
            out <- data.table(t(ti$value))
            out[,3] <- ti$value[[3]][[1]] %+% "," %+% ti$value[[3]][[2]] %+% "," %+% ti$value[[3]][[3]]
            out[,4] <- ti$value[[4]][[1]] %+% "," %+% ti$value[[4]][[2]] %+% "," %+% ti$value[[4]][[3]]
            setnames(out, c("account0Id","account1Id","token0Id","token1Id","token0Amount","token1Amount","burnAmount"))
            dex_RemoveLiquidity_list[[i]] <- data.table(core_data[i], out)
          } else if (core_data[i, event_id] == "AssetSwap") {
            assetIndex <- as.character(ti$value[[3]][[1]])
            assetType <- as.character(ti$value[[3]][[2]])
            chainId <- as.character(ti$value[[3]][[3]])
            balances <- as.character(ti$value[[4]])
            out <- data.table(ti$value[[1]], ti$value[[1]],
                              assetIndex[1] %+% "," %+%  assetType %+% "," %+%  chainId[1],
                              assetIndex[2] %+% "," %+%  assetType %+% "," %+%  chainId[2],
                              assetIndex[3] %+% "," %+%  assetType %+% "," %+%  chainId[3],
                              assetIndex[4] %+% "," %+% assetType %+% "," %+%  chainId[4],
                              balances[1],
                              balances[2],
                              balances[3],
                              balances[4])
            setnames(out, c("owner","recipient","token0Id","token1Id","token2Id","token3Id","token0Amount","token1Amount","token2Amount","token3Amount"))
            dex_Swap_list[[i]] <- data.table(core_data[i], out)
          }
        } # end dex_Swap
      } # end for

  } # end if

  out <- list()

  if (any(core_data$module_id %in% c('dex','zenlinkprotocol'))) {
    if (length(dex_AddLiquidity_list) > 0) {
      dex_AddLiquidity <- rbindlist(dex_AddLiquidity_list)
    } else {
      dex_AddLiquidity <- NULL
    }

    if (length(dex_RemoveLiquidity_list) > 0) {
      dex_RemoveLiquidity <- rbindlist(dex_RemoveLiquidity_list)
    } else {
      dex_RemoveLiquidity <- NULL
    }

    if (length(dex_Swap_list) > 0) {
      dex_Swap <- rbindlist(dex_Swap_list)
    } else {
      dex_Swap <- NULL
    }
    out <- list(out,
                dex_Swap = dex_Swap,
                dex_RemoveLiquidity = dex_RemoveLiquidity,
                dex_AddLiquidity = dex_AddLiquidity)

  }

  if (any(core_data$module_id %in% c('balances'))) {
    if (length(balances_Deposit_list) > 0) {
      balances_Deposit <- rbindlist(balances_Deposit_list)
    } else {
      balances_Deposit <- NULL
    }

    if (length(balances_Withdraw_list) > 0) {
      balances_Withdraw <- rbindlist(balances_Withdraw_list)
    } else {
      balances_Withdraw <- NULL
    }

    if (length(balances_Transfer_list) > 0) {
      balances_Transfer <- rbindlist(balances_Transfer_list)
    } else {
      balances_Transfer <- NULL
    }
    out <- list(out,
                balances_Deposit = balances_Deposit,
                balances_Withdraw = balances_Withdraw,
                balances_Transfer = balances_Transfer)

  }

  if (any(core_data$module_id %in% c('loans'))) {
    if (length(loans_ConfiscateCollateralAndDebit_list) > 0) {
      loans_ConfiscateCollateralAndDebit <- rbindlist(loans_ConfiscateCollateralAndDebit_list)
    } else {
      loans_ConfiscateCollateralAndDebit <- NULL
    }

    if (length(loans_PositionUpdated_list) > 0) {
      loans_PositionUpdated <- rbindlist(loans_PositionUpdated_list)
    } else {
      loans_PositionUpdated <- NULL
    }
    out <- list(out,
                loans_ConfiscateCollateralAndDebit = loans_ConfiscateCollateralAndDebit,
                loans_PositionUpdated = loans_PositionUpdated)

  }

  if (any(core_data$module_id %in% c('currencies'))) {
    if (length(currencies_Transferred_list) > 0) {
      currencies_Transferred <- rbindlist(currencies_Transferred_list)
    } else {
      currencies_Transferred <- NULL
    }

    if (length(currencies_Deposited_list) > 0) {
      currencies_Deposited <- rbindlist(currencies_Deposited_list)
    } else {
      currencies_Deposited <- NULL
    }

    if (length(currencies_Withdrawn_list) > 0) {
      currencies_Withdrawn <- rbindlist(currencies_Withdrawn_list)
    } else {
      currencies_Withdrawn <- NULL
    }
    out <- list(out,
                currencies_Transferred = currencies_Transferred,
                currencies_Deposited = currencies_Deposited,
                currencies_Withdrawn = currencies_Withdrawn)

  }

  if (any(core_data$module_id %in% c('incentives'))) {
    if (length(incentives_DepositDexShare_list) > 0) {
      incentives_DepositDexShare <- rbindlist(incentives_DepositDexShare_list)
    } else {
      incentives_DepositDexShare <- NULL
    }

    if (length(incentives_WithdrawDexShare_list) > 0) {
      incentives_WithdrawDexShare <- rbindlist(incentives_WithdrawDexShare_list)
    } else {
      incentives_WithdrawDexShare <- NULL
    }

    if (length(incentives_ClaimRewards_list) > 0) {
      incentives_ClaimRewards <- rbindlist(incentives_ClaimRewards_list)
    } else {
      incentives_ClaimRewards <- NULL
    }
    out <- list(out,
                incentives_DepositDexShare = incentives_DepositDexShare,
                incentives_WithdrawDexShare = incentives_WithdrawDexShare,
                incentives_ClaimRewards = incentives_ClaimRewards)

  }

  if (any(core_data$module_id %in% c('treasury'))) {
    if (length(treasury_Deposited_list) > 0) {
      treasury_Deposited <- rbindlist(treasury_Deposited_list)
    } else {
      treasury_Deposited <- NULL
    }
    out <- list(out,
                treasury_Deposited = treasury_Deposited)

  }

  if (any(core_data$module_id %in% c('auctionmanager'))) {
    if (length(auctionmanager_DEXTakeCollateralAuction_list) > 0) {
      auctionmanager_DEXTakeCollateralAuction <- rbindlist(auctionmanager_DEXTakeCollateralAuction_list)
    } else {
      auctionmanager_DEXTakeCollateralAuction <- NULL
    }
    if (length(auctionmanager_CollateralAuctionDealt_list) > 0) {
      auctionmanager_CollateralAuctionDealt <- rbindlist(auctionmanager_CollateralAuctionDealt_list)
    } else {
      auctionmanager_CollateralAuctionDealt <- NULL
    }

    out <- list(out,
                auctionmanager_DEXTakeCollateralAuction = auctionmanager_DEXTakeCollateralAuction,
                auctionmanager_CollateralAuctionDealt = auctionmanager_CollateralAuctionDealt)
  }

  if (any(core_data$module_id %in% c('cdpengine'))) {
    if (length(cdpengine_LiquidateUnsafeCDP_list) > 0) {
      cdpengine_LiquidateUnsafeCDP <- rbindlist(cdpengine_LiquidateUnsafeCDP_list)
    } else {
      cdpengine_LiquidateUnsafeCDP <- NULL
    }

    out <- list(out,
                cdpengine_LiquidateUnsafeCDP = cdpengine_LiquidateUnsafeCDP)
  }

  out

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
#' get_subscan_metadata(network = 'Acala')
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
#' get_subscan_price()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_price <- function(network = 'Polkadot', time = 957105) {

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
#' get_subscan_currencies('Polkadot')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_subscan_currencies <- function(network = 'Polkadot') {

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
