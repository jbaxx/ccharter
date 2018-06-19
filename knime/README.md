
Integration with KNIME: ccharter
================================

I'll show you how the `ccharter` function can be integrated in a KNIME workflow. This is a work in progress, currently will require a bit of tweaking to make it work with your dataset.

We'll be using use the nodes in the picture below for this example.

![knime flow](plots/knime_workflow.PNG)

1.  Ensure you have installed the `ccharter` package in R and that KNIME uses your own R installation
    -   Package [installation instructions](https://github.com/jbaxx/ccharter)
    -   Details for using R installation with KNIME are out of the scope of this guide

2.  In a new KNIME workflow, create an *R Source (Table)* node. Press *F6* to configure the node.
3.  This node let's you read data from many sources, we pass the data read to the output port assigning it to a variable named *knime.out*. For this example we'll use a random generated dataset. You may copy/paste the code in the node.
    -   Since R in KNIME has some issues passing date values, we'll always pass dates as characters to the output port.

``` r
  set.seed(154)
  
  time.series <- data.frame(t.dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
                 t.values = c(
                 seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
                 seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
                 seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
                 seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
                 seq(0.1, 0.5, by = 0.1) * runif(5) + 4)
                 )
  time.series$t.dates <- as.character(time.series$t.dates)
  
  knime.out <- time.series # assign your data frame here
  
```

<ol start="4">
<li>
Create an <i>R Snippet</i> node:
</li>
</ol>
-   Here we call the `ccharter` library
-   Execute the ccpoints function to calculate the Control Systems
-   Extract the resulting data frame from the ccpoints object
-   Send the data frame to the output port

``` r
library(ccharter)

knime.in$"t.dates" <- as.Date(knime.in$"t.dates")

control.chart.data <- ccpoints(knime.in, "t.dates", "t.values")
control.chart.data[["data"]]$t.dates <- as.character(control.chart.data[["data"]]$t.dates)

knime.out <- control.chart.data[["data"]]
```

<ol start="5">
<li>
Create an <i>R View (Table)</i> node:
</li>
</ol>
-   With this node we're going to plot the results
-   Since we're not getting the ccpoints object from the previous node, we can't use the cc2plot function
-   As workaround copy/paste the chunk of code below, it will plot the data frame received.

``` r
library(ggplot2)
library(scales)

knime.in$"t.dates" <- as.Date(knime.in$"t.dates")

  g <- ggplot(knime.in, aes(x = t.dates))
  g <- g + geom_line(aes(y = data.mean), size = 1.2, color = "darkorange")
  g <- g + geom_line(aes(y = data.ll), size = 1.2, color = "steelblue")
  g <- g + geom_line(aes(y = data.ul), size = 1.2, color = "steelblue")
  g <- g + geom_line(aes(y = t.values), size=0.6, color = "gray44")
  g <- g + geom_point(aes(y = t.values), color = "midnightblue")
  g <- g + theme_bw()
  g <- g + scale_x_date(labels = date_format("%b/%y"), minor_breaks = NULL, breaks = date_breaks("month"))
  g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  g <- g + labs(x = "Month")
  g <- g + theme(plot.title = element_text(hjust = 0))
  print(g)
```

<ol start="5">
<li>
To view the plot, right-click the <i>R View (Table)</i> node and select <i>View R: View\_:</i>
</li>
</ol>  
  
![knime flow](plots/knime_workflow_view.PNG)

<ol start="6">
<li>
Your result may look like this:
</li>
</ol>
  
![knime flow](plots/knime_workflow_view_plot.PNG)

<ol start="6">
<li>
You may need to change the png settings in the <i>R View (Table)</i> node to set the desired dimensions
</li>
</ol>
