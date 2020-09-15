library(openair)

timep <- function(data, pollutants, window) {
    timestamp <- as.POSIXct(window$center, tz="GMT")

    chart <- timePlot(data,
                      cols = "hue",
                      group = FALSE,
                      key.columns = 5,
                      lwd = 1.5,
                      pollutant = pollutants,
                      windflow = list(col = "grey50", lwd = 1, scale = 0.05, length=0.05),
                      ref.x = (list(v = timestamp, col = "grey70", lty = 2, lwd = 1.5)),
                      y.relation = "free")

    chart$plot
}

corrp <- function(data, pollutants) {
    chart <- corPlot(data, cols = "default", pollutant = pollutants)
    chart$plot
}

prosep <- function(data, pollutant) {
    data$dayHour <- format(data$date, "%Y-%m-%d %H:00")

    chart <- pollutionRose(data,
                           angle=22.5,
                           annotate = FALSE,
                           cols = "default",
                           grid.line = 25,
                           key.position = "right",
                           layout = c(5, 5),
                           paddle = FALSE,
                           pollutant = pollutant,
                           type = "dayHour")
    chart$plot
}

windp <- function(data) {
    data$dayHour <- format(data$date, "%Y-%m-%d %H:00")

    chart <- windRose(data,
                      angle=22.5,
                      breaks = c(0, 3, 6, 9, 12),
                      cols = "default",
                      grid.line = 25,
                      key.footer = "(km/h)",
                      key.position = "right",
                      layout = c(5, 5),
                      paddle = FALSE,
                      type = "dayHour")

        chart$plot
}
