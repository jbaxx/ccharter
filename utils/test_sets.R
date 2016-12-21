# Create a random time series
set.seed(154)
time.series <- data.frame(t.dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
                          t.values = c(
                            seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
                            seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
                            seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
                            seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
                            seq(0.1, 0.5, by = 0.1) * runif(5) + 4)
)

# Execute function
control.chart.data <- ccpoints(time.series, "t.dates", "t.values")
print(control.chart.data)

windows()
cc2plot(control.chart.data)
# To extract only the data frame
control.chart.data[["data"]]

time.series

t <- cbind(eje = c(1:nrow(time.series)), time.series)
t <- t[,-2]
q <- as.data.frame(cbind(c(1,2,3,4,5,6,7), c(3.03, 3.20, 3.03, 3.20, 3.03, 3.20, 3.03)))
colnames(q) <- c("eje", "t.values")
w <- rbind(q,t)
e <- cbind(1:nrow(w), w)
e <- e[, -2]
colnames(e)[1] <- "eje"
head(e)

control.chart.data <- ccpoints2(e, "eje", "t.values", date.type = F)
print(control.chart.data)
windows()
cc2plot(control.chart.data)

data <- control.chart.data[["data"]]





for (i in 1:length(unique(data$data.mean))){
  d_set <- data[data$data.mean == unique(data$data.mean)[i],]
  data[data$data.mean == unique(data$data.mean)[i], c("data.mean")] <- mean(d_set$t.values)
  data[data$data.mean == unique(data$data.mean)[i], c("data.ll")] <- mean(d_set$t.values) - sd(d_set$t.values)
  data[data$data.mean == unique(data$data.mean)[i], c("data.ul")] <- mean(d_set$t.values) + sd(d_set$t.values)
  rm(d_set)
}


library(plot)
cc3plot <- function(data, data.title = "") {
  if (class(data) != "ccpoints") {
    stop("Expecting a ccpoints class object \n Details in documentation at ?cc2plot")
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
  } else if(inherits(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])], "numeric") |
            inherits(data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])], "integer")){
    g <- g + ggplot2::scale_x_continuous(breaks = data[["data"]][, which(colnames(data[["data"]]) %in% data[["dates.name"]])] )#minor_breaks = NULL)
  }
  g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  g <- g + ggplot2::labs(x = "Observations")
  g <- g + ggplot2::ggtitle(data.title)
  g <- g + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
  print(g)
}



