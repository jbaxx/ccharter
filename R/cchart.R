#' Calculates Control Systems points
#' @description
#' Control Charts made easy.
#' Calculates Control Systems for a time series.
#' The current distrubution uses two Control Rules to define systems:
#' 1. Mean Rule: Six continuous points above or below mean. See points.vs.avg argument.
#' 2. Standard Deviation Rule: Four points above or below SD. See points.vs.sd argument.
#'
#' @usage
#' ccpoints(data, dates, values, points.vs.avg = 6, points.vs.sd = 4)
#' @param data The input Data Frame containing the time series.
#' @param dates Column name for date values. Class should be date,
#' if not, will attempt to coerce using lubridate::mdy(dates),
#' assuming format is month-day-year.
#' @param values Column name for data points values. Class should be numeric,
#' if not, will attempt to coerce.
#' @param points.vs.avg For Mean Rule, establishes how many continuous points
#' should be above or below mean to define a system. Default to 6.
#' @param points.vs.sd For Standard Deviation Rule, establises how many continuous points
#' should be above or below two standard deviations to define a new system. Default to 4.
#' @return The function will return an object of class ccpoints.
#' A list including the data frame with the system points, and column names of the time series.\cr
#' The data frame will include the submited time series and the followin new columns: \cr\cr
#' data.mean: the mean value for each system \cr
#' data.ll: the lower standard deviation of each system \cr
#' data.ul: the upper standard deviation of each system \cr
#'
#' @examples
#' # Create a random time series
#' set.seed(154)
#' time.series <- data.frame(t.dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
#'                t.values = c(
#'                seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
#'                seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
#'                seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
#'                seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
#'                seq(0.1, 0.5, by = 0.1) * runif(5) + 4)
#'                )
#'
#' # Execute function
#' control.chart.data <- ccpoints(time.series, "t.dates", "t.values")
#' print(control.chart.data)
#'
#' # To extract only the data frame
#' control.chart.data[["data"]]
#'
#'@export
ccpoints <- function(data, dates, values, points.vs.avg = 6, points.vs.sd = 4) {

  if (class(data[, dates]) != "Date"){
    data[, dates] <- lubridate::mdy(data[, dates])
  }

  if (class(data[, values]) != "numeric"){
    data[, values] <- as.numeric(data[, values])
  }

  # Order dataset in ascendant dates
  data <- data[order(data[, dates]), ]
  # Calculate mean and standard deviation of first break (first six values)
  data.mean <- mean(data[, values][1:6])
  data.sd   <-   sd(data[, values][1:6])
  # Create columns to store breaks' mean, standard deviation lower and upper limits
  data$data.mean <- 0
  data$data.ll <- 0
  data$data.ul <- 0
  # Initialize values for first break
  data$data.mean[1:6] <- data.mean
  data$data.ll[1:6] <- data.mean - data.sd
  data$data.ul[1:6] <- data.mean + data.sd
  # Counters to track break points along dataset
  data$new.count <- 0
  data$new.count.sd <- 0
  # Internal counters for mean rule
  count.p <- 0
  count.n <- 0
  # Internal counters for standard deviation rule
  count.sd.p <- 0
  count.sd.n <- 0


  for (i in 7:nrow(data)) {
    # Store mean and sd for current point in the loop
    data$data.mean[i] <- data.mean
    data$data.ll[i] <- data.mean - data.sd
    data$data.ul[i] <- data.mean + data.sd

    # MEAN RULE: SIX CONTINUOUS POINTS ABOVE OR BELOW MEAN
    # If point above mean, upper counter + 1 and reset lower counter
    if (data[, values][i] > data.mean ) {
      count.p <- count.p + 1
      data$new.count[i] <- count.p
      count.n <- 0
      # If point below mean, lower counter + 1 and reset upper counter
    } else if (data[, values][i] < data.mean ) {
      count.n <- count.n + 1
      data$new.count[i] <- count.n
      count.p <- 0
    }
    # If one of the counters have reached the established break point
    # Then all these past points represents a new system
    # Calculate mean and standard deviation of this system
    if (count.p == points.vs.avg | count.n == points.vs.avg) {
      data.mean <- mean(data[, values][(i - (points.vs.avg-1)):i])
      if (i < nrow(data)) {
        data.sd <- sd(data[, values][(i - (points.vs.avg-1)):i])
      }
      # Rewrite mean and standard deviation values for this new system
      data$data.mean[(i - (points.vs.avg-1)):i] <- data.mean
      data$data.ll[(i - (points.vs.avg-1)):i] <- data.mean - data.sd
      data$data.ul[(i - (points.vs.avg-1)):i] <- data.mean + data.sd
      # Reset all the counters
      count.p <- 0
      count.n <- 0
      count.sd.p <- 0
      count.sd.n <- 0
    }

    # STANDARD DEVIATION RULE: FOUR POINTS ABOVE OR BELOW TWO STANDARD DEVIATIONS
    # If point above 2 SD, upper counter + 1 and reset lower counter
    if (data[, values][i] > (data.mean + 2 * data.sd) & data.mean != data$data.ul[i]) {
      count.sd.p <- count.sd.p + 1
      data$new.count.sd[i] <- count.sd.p
      count.sd.n <- 0
    }
    # If point below 2 SD, lower counter + 1 and reset upper counter
    if (data[, values][i] < (data.mean - 2 * data.sd ) & data.mean != data$data.ll[i]) {
      count.sd.n <- count.sd.n + 1
      data$new.count.sd[i] <- count.sd.n
      count.sd.p <- 0
    }
    # If one of the counters have reached the established break point
    # Then all these past points represents a new system
    # Calculate mean and standard deviation of this system
    #print(points.vs.sd)
    #print(points.vs.sd - 1)
    if (count.sd.p == points.vs.sd | count.sd.n == points.vs.sd) {
      data.mean <- mean(data[, values][(i - (points.vs.sd -1)):i])
      if (i < nrow(data)) {
        data.sd <- sd(data[, values][(i - (points.vs.sd -1)):i])
      }
      # Rewrite mean and standard deviation values for this new system
      data$data.mean[(i - (points.vs.sd -1)):i] <- data.mean
      data$data.ll[(i - (points.vs.sd -1)):i] <- data.mean-data.sd
      data$data.ul[(i - (points.vs.sd -1)):i] <- data.mean+data.sd
      # Reset all the counters
      count.sd.p <- 0
      count.sd.n <- 0
      count.p <- 0
      count.n <- 0
    }
  }
  l<-list()
  l[["data"]] <- data[, which(names(data) %in% c(dates, values, "data.mean", "data.ll", "data.ul"))]
  l[["dates.name"]] <- dates
  l[["values.name"]] <- values
  class(l) <- c("ccpoints")
  return(l)
}

#' Plot a Control Chart.
#' @description
#' Plots a Control Chart. Receives as input a ccpoints object.
#' @usage
#' cc2plot (data, data.title = "")
#' @param data A ccpoints object. See ?ccharter::ccpoints function for reference.
#' @param data.title Title for the Control Chart plot.
#'
#' @examples
#' # Create a random time series
#' set.seed(154)
#' time.series <- data.frame(t.dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
#'                t.values = c(
#'                seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
#'                seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
#'                seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
#'                seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
#'                seq(0.1, 0.5, by = 0.1) * runif(5) + 4)
#'                )
#'
#' # Execute function
#' control.chart.data <- ccpoints(time.series, "t.dates", "t.values")
#'
#' # Create chart
#' cc2plot(control.chart.data)
#'
#'
#'@export
#'
cc2plot <- function(data, data.title = "") {
  if (class(data) != "ccpoints") {
    stop("Expecting a ccpoints class object \n Details in documentation at ?cc2plot")
  }
  #windows(width = 11, height = 5)
  g <- ggplot2::ggplot(data[["data"]], ggplot2::aes_string(x = data[["dates.name"]]))
  g <- g + ggplot2::geom_line(ggplot2::aes_string(y = data[["values.name"]]), size=1, color = "gray44")
  g <- g + ggplot2::geom_point(ggplot2::aes_string(y = data[["values.name"]]), color = "midnightblue")
  g <- g + ggplot2::geom_line(ggplot2::aes(y = data.mean), size = 1.2, color = "darkorange")
  g <- g + ggplot2::geom_line(ggplot2::aes(y = data.ll), size = 1.2, color = "steelblue")
  g <- g + ggplot2::geom_line(ggplot2::aes(y = data.ul), size = 1.2, color = "steelblue")
  g <- g + ggplot2::theme_bw()
  g <- g + ggplot2::scale_x_date(labels = scales::date_format("%b/%y"), minor_breaks = NULL, breaks = scales::date_breaks("month"))
  g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  g <- g + ggplot2::labs(x = "Month")
  g <- g + ggplot2::ggtitle(data.title)
  g <- g + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
  print(g)
}



#set.seed(154)
#rev <- data.frame(dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
#                  val= c(seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
#                        seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
#                       seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
#                       seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
#                       seq(0.1, 0.5, by = 0.1) * runif(5) + 4))
#
#cc2plot(ccpoints(rev, "dates", "val"))
