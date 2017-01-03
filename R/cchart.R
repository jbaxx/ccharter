#' Calculates Control Systems points
#' @description
#' Control Charts made easy.
#' Calculates Control Systems for a series.
#' The current distribution uses the following Control Rules to define systems:
#' 1. Mean Rule: N continuous points above or below mean. See points.vs.avg argument.
#' 2. Out of Control Points Rule: Points above or below 3 Standard Deviation are excluded from calculations.
#'
#' @usage
#' ccpoints(data, dates, values, points.vs.avg = 7,
#'          date.type = TRUE, already.ordered = FALSE)
#' @param data The input Data Frame.
#' @param dates Column name for date or ordering variable.
#' @param values Column name for data points values. Class should be numeric,
#' if not, will attempt to coerce.
#' @param points.vs.avg For Mean Rule, establishes how many continuous points
#' should be above or below mean to define a system. Default to 6.
#' @param date.type States if observations are based on dates. Defaults to TRUE. If TRUE, will attempt to coerce
#' to a date format using extensive use of lubridate package dates formats.
#' @param already.ordered Tells the function if the data is already ordered. Defaults to FALSE.
#' The function will attempt to order it.
#' @return The function will return a list object with class ccpoints.
#' A list including the data frame with the system points, and column names of the series.\cr
#' The data frame will include the submited series and the following new columns: \cr\cr
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
ccpoints <- function(data, dates, values, points.vs.avg = 7,
                      date.type = TRUE, already.ordered = FALSE) {

  if(nrow(data) <= 12){
    stop("Not enough data points, 12 or less supplied")
  }

  if(nrow(data) > 12 & nrow(data) <= 20){
    warning("Between 12 and 20 data points supplied, function executed but may not return system breaks")
  }

  ######
  #1. Data Preparation Begins
  ######
  if(date.type){
    #DATE DATA PREPARATION
    if (class(data[, dates]) != "Date"){
      fechas <- tryCatch(lubridate::ymd( data[, dates]), warning = function(e) "warning")

      if(class(fechas) == "character") {
        fechas <- tryCatch(lubridate::mdy( data[, dates]), warning = function(e) "warning")
      } else if(class(fechas) == "character") {
        fechas <- tryCatch(lubridate::ymd( data[, dates]), warning = function(e) "warning")
      } else if(class(fechas) == "character"){
        fechas <- tryCatch(lubridate::myd( data[, dates]), warning = function(e) "warning")
      } else if(class(fechas) == "character"){
        fechas <- tryCatch(lubridate::dmy( data[, dates]), warning = function(e) "warning")
      } else if(class(fechas) == "character"){
        fechas <- tryCatch(lubridate::dym( data[, dates]), warning = function(e) "warning")
      }

      if (class(fechas) == "character"){
        stop("Dates format not supported. Please review function documentation: ?ccpoints.
             If you're supplying a non-date column, change date.type parameter to FALSE")
      }


      data[, dates] <- fechas
      rm(fechas)
      }
  }

  if(!already.ordered){
    # Order dataset in ascendant dates
    data <- data[order(data[, dates]), ]
  }

  #NUMERIC DATA PREPARATION
  #Remove commas if present
  data[, values] <- as.numeric(gsub(",", "", data[, values]))

  #Data handling for values data points, specially for factors coercion
  if (!is.numeric(data[, values])){
    if(is.factor(data[, values])){
      data[, values] <- as.numeric(levels(data[, values]))[data[, values]]
    } else {
      data[, values] <- as.numeric(data[, values])
    }
  }

  #MISSING VALUES HANDLING
  if(nrow(data[!complete.cases(data[, c(dates, values)]), ]) > 0){
    warning("Data Frame contained missing values and were removed.")
    missing_values <- data[!complete.cases(data[, c(dates, values)]), ]
    data <- data[complete.cases(data[, c(dates, values)]), ]
  } else {missing_values <- NA}

  ######
  #1. Data Preparation Ends
  ######

  ######
  #2. Control Systems Calculation Begins
  ######

  # Calculate mean and standard deviation of ALL DATASET
  data.mean <- mean(data[, values])
  data.sd   <-   sd(data[, values])
  # Locate Out of Control points
  data$ooc <- TRUE
  data$ooc[which(data[, values] > data.mean + 3 * data.sd | data[, values] < data.mean - 3 * data.sd)] <- FALSE
  # Recalculate mean and sd all dataset, without evaluating Out of Control points
  data.mean <- mean(data[, values][data$ooc])
  data.sd   <-   sd(data[, values][data$ooc])
  # Create columns to store breaks' mean, standard deviation lower and upper limits
  data$data.mean <- 0
  data$data.ll <- 0
  data$data.ul <- 0
  # Counters to track break points along dataset
  data$new.count <- 0
  # Internal counters for mean rule
  count.p <- 0
  count.n <- 0


  for (i in 1:nrow(data)) {
    # Store mean and sd for current point in the loop
    data$data.mean[i] <- data.mean
    data$data.ll[i] <- data.mean - data.sd
    data$data.ul[i] <- data.mean + data.sd

    # MEAN RULE: CONTINUOUS POINTS ABOVE OR BELOW MEAN
    # If point above mean, upper counter + 1 and reset lower counter
    if (data[, values][i] > data.mean & data[, "ooc"][i] ) {
      count.p <- count.p + 1
      data$new.count[i] <- count.p
      count.n <- 0
      # If point below mean, lower counter + 1 and reset upper counter
    } else if (data[, values][i] < data.mean & data[, "ooc"][i]  ) {
      count.n <- count.n + 1
      data$new.count[i] <- count.n
      count.p <- 0
    } else {
      if(count.p > count.n) {data$new.count[i] <- count.p}
      if(count.p < count.n) {data$new.count[i] <- count.n}
    }
    # If one of the counters have reached the established break point
    # Then all these past points represents a new system
    # Calculate mean and standard deviation of this system
    if (count.p == points.vs.avg | count.n == points.vs.avg) {
      data.mean <- mean(data[, values][(i - (points.vs.avg - 1)):i][data$ooc[(i - (points.vs.avg - 1)):i]])
      if (i < nrow(data)) {
        data.sd <- sd(data[, values][(i - (points.vs.avg - 1)):i][data$ooc[(i - (points.vs.avg - 1)):i]])
      }
      # Rewrite mean and standard deviation values for this new system
      alpha <- sum(as.numeric(!data$ooc[(i - (points.vs.avg - 1)):i]))
      data$data.mean[( (i - alpha) - (points.vs.avg - 1)):i] <- data.mean
      data$data.ll[((i - alpha) - (points.vs.avg - 1)):i] <- data.mean - data.sd
      data$data.ul[((i - alpha) - (points.vs.avg - 1)):i] <- data.mean + data.sd
      # Reset all the counters
      count.p <- 0
      count.n <- 0
    }

  }

  # Function Return

  # Recalculate limits for each system
  for (i in 1:length(unique(data$data.mean))){
    d_set <- data[data$data.mean == unique(data$data.mean)[i],]
    data[data$data.mean == unique(data$data.mean)[i], c("data.mean")] <- mean(d_set[, values][d_set$ooc])
    data[data$data.mean == unique(data$data.mean)[i], c("data.ll")] <- mean(d_set[, values][d_set$ooc]) - sd(d_set[, values][d_set$ooc])
    data[data$data.mean == unique(data$data.mean)[i], c("data.ul")] <- mean(d_set[, values][d_set$ooc]) + sd(d_set[, values][d_set$ooc])
    rm(d_set)
  }

  # Appending return metadata
  l<-list()
  l[["data"]] <- data#[, which(names(data) %in% c(dates, values, "data.mean", "data.ll", "data.ul"))]
  l[["dates.name"]] <- dates
  l[["values.name"]] <- values
  l[["systems_count"]] <- length(unique(data$data.mean))
  l[["missing_values"]] <- missing_values

  # Subset only last system
  last <- data[which(data$data.mean == unique(data$data.mean)[length(unique(data$data.mean))]), ]
  l[["point_last_break"]] <- last[1, dates]
  if(date.type){
    l[["weeks_since_last_break"]] <- floor(difftime(Sys.Date(), last[1, dates], units = "weeks"))
  } else {
    l[["weeks_since_last_break"]] <- "No Date data type provided"
  }

  next_break <- c("Next system expected to break positive", "Next system expected to break negative")
  l[["next_break"]] <- next_break[as.numeric(count.p < count.n) + 1]

  next_break_mean <- c(points.vs.avg - count.p, points.vs.avg - count.n)

  l[["next_break_values"]] <- c("Continous points vs mean: " = next_break_mean[as.numeric(count.p < count.n) + 1])


  class(l) <- append(class(l), "ccpoints")
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
  if (!("ccpoints" %in% class(data))) {
    stop("Expecting a ccpoints class object \n Details in documentation at ?cc2plot")
  }

  # Store characters values, and create a numeric axis to plot
  if(inherits(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])], "character")){
    data[["data"]]$sust_axis <- data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])]
    data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])] <- as.numeric(c(1:length(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])])))

  }

  #windows(width = 11, height = 5)
  g <- ggplot2::ggplot(data[["data"]], ggplot2::aes_string(x = data[["dates.name"]]))
  g <- g + ggplot2::geom_line(ggplot2::aes(y = data.mean), size = 1.2, color = "darkorange")
  g <- g + ggplot2::geom_line(ggplot2::aes(y = data.ll), size = 1.2, color = "steelblue")
  g <- g + ggplot2::geom_line(ggplot2::aes(y = data.ul), size = 1.2, color = "steelblue")
  g <- g + ggplot2::geom_line(ggplot2::aes_string(y = data[["values.name"]]), size=0.6, color = "gray44")
  g <- g + ggplot2::geom_point(ggplot2::aes_string(y = data[["values.name"]]), color = "midnightblue")
  g <- g + ggplot2::theme_bw()
  if(inherits(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])], "Date")){
    g <- g + ggplot2::scale_x_date(labels = scales::date_format("%b/%y"), minor_breaks = NULL, breaks = scales::date_breaks("month"))
  } else if(inherits(data[["data"]]$sust_axis, "character")){
    g <- g + ggplot2::scale_x_continuous(breaks = data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])],
                                         labels = c(data[["data"]]$sust_axis))
  } else if(inherits(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])], "numeric") |
            inherits(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])], "integer")){
    g <- g + ggplot2::scale_x_continuous(breaks = data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])])
  }
  g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  g <- g + ggplot2::labs(x = "Observations")
  g <- g + ggplot2::ggtitle(data.title)
  g <- g + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
  print(g)
}

