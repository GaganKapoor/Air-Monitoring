library(dplyr)
library(httr)
library(lubridate)
library(stringr)
library(tidyr)

datefmt <- function(datetime) {
    ## Format a POSIXct datetime for the WBEA API

    paste(year(datetime),
          month(datetime) - 1,
          day(datetime),
          hour(datetime),
          minute(datetime), sep = ",")
}

wbea_request <- function(ids, start, end) {
    ## Request data from the wbea continuous data viewer

    qstring <- list(a = "wbe",
                    c = paste(ids, collapse = ","),
                    s = datefmt(start),
                    e = datefmt(end))

    r <- httr::GET("http://67.210.212.45/silverdata2", query = qstring, verbose())
    data <- strsplit(httr::content(r, "text"), "<!>")

    data[[1]][-1]
}

extract <- function(response, pattern, separator, names) {
    ## Extract data from response string

    data <- str_match(response, pattern = pattern)[, 2]
    l <- strsplit(data, separator, fixed = T) %>%
        lapply(function(x) {if (is.null(x) | length(x) == 0) {NA} else { x }}) ## Catch empty flags
    df <- data.frame(matrix(unlist(l), nrow = length(l), byrow = T), stringsAsFactors = F)
    names(df) <- names
    df
}

parse <- function(response) {
    ## Parse response string to openair format

    header <- extract(response, "^(.*)<l>", "+", c("id", "measurement", "station", "units"))
    dates <- extract(response, "<l>(.*)<d>", ",", c("n", "step", "start", "nodata"))
    data <- extract(response, "<d>(.*)<f>", ";;", c("data"))
    flags <- extract(response, "<f>(.*)", ";;", c("flags"))

    df <- cbind(header, dates, data, flags)
    df$station <- str_trim(df$station)
    df$n <- as.numeric(df$n)
    df$step <- as.numeric(df$step)
    df$start <- ymd_hm(df$start)

    df <- df %>%
        separate_rows(data, flags, sep=",") %>%
        mutate(masked_data = as.numeric(ifelse(flags == 0, data, NA))) %>%
        group_by(id) %>%
        mutate(timestamp = start + ((seq(n()) - 1) * step)) %>%
        filter(timestamp < start + (n * step)) %>%
        select(-c(n, step, start, nodata))

    stations <- split(df, df$station)

    lapply(stations, function(station) {
        pivot_wider(station,
                    id_cols = timestamp,
                    names_from = id,
                    values_from = masked_data)
       }
    )
}
