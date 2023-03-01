library(httr)
library(jsonlite)


#' @export
getProtocols <-function() {
  baseurl <- 'https://api.llama.fi/protocols'
  r <- GET(url = baseurl)
  tmp <- content(r, as="text", encoding="UTF-8") %>%
    fromJSON(flatten=TRUE) %>%
    as.data.table
  rd <- melt(tmp, id=1:4, measure = grep("chainTvls", names(tmp)))
  rd <- rd[!is.na(value)]
  rd[, tag := gsub("chainTvls.","", variable)]
  utags <- unique(rd$tag)
  d <- strsplit(utags, "-") %>%
    lapply(`[[`, 1) %>%
    unlist
  sort(unique(d))
}

#' @export
getTVL <- function(defi) {
  for (id in defi) {
    baseurl <- 'https://api.llama.fi/v2/historicalChainTvl/' %+% id
    r <- GET(url = baseurl)
    stopifnot(r$status_code == 200)
    tmp <- content(r, as="text", encoding="UTF-8") %>%
      fromJSON(flatten=TRUE) %>%
      as.data.table
    tmp[, date := as.POSIXct(date, origin = "1970-01-01")]
    tmp[, chain := id]
    tmp
  }
}
