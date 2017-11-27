#' Reproducible ggplot2 chart
#'
#' @param data a data to plot using ggplot2
#' @param xvar a variable to place in the aes() as x axis
#' @param yvar a variable to place in the aes() as y axis
#' @param which_geom
#' @param legend_nm
#' @param what_unit
#'
#' @return predefined form of graph
#' @export
given_plot <- function(data, xvar, yvar, which_geom, legend_nm, what_unit) {

  library(ggplot2, quietly = TRUE)
  library(lazyeval, quietly = TRUE)

  data_gd <- NULL
  data_gd$xvar <- tryCatch(expr = lazyeval::lazy_eval(substitute(xvar), data = data),
                           error = function(e) eval(envir = data, expr = parse(text = xvar)))
  data_gd$yvar <- tryCatch(expr = lazyeval::lazy_eval(substitute(yvar), data = data),
                           error = function(e) eval(envir = data, expr = parse(text = yvar)))


  if (data_gd$xvar %>% is.factor) {

    ggplot(data = as.data.frame(data_gd), mapping = aes(x = xvar, y = yvar)) +
      eval(parse(text = which_geom)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.title = element_blank(), legend.justification = c(1, 0),
            legend.position = c(1, 0), legend.background = element_blank()) +
      annotate("text", x = length(data_gd$xvar), y = max(data_gd$yvar), hjust = 1, vjust = 2, label = what_unit) +
      annotate("text", x = length(data_gd$xvar), y = max(data_gd$yvar), hjust = 1, label = legend_nm)

  } else {

    ggplot(data = as.data.frame(data_gd), mapping = aes(x = xvar, y = yvar)) +
      eval(parse(text = which_geom)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%y/%m/%d") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.title = element_blank(), legend.justification = c(1, 0),
            legend.position = c(1, 0), legend.background = element_blank()) +
      annotate("text", x = min(data_gd$xvar), y = max(data_gd$yvar), hjust = 0.1, vjust = 2, label = what_unit) +
      annotate("text", x = min(data_gd$xvar), y = max(data_gd$yvar), hjust = 0.1, label = legend_nm)

  }
}
