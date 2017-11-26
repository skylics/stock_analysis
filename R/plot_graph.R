#' Reproducible ggplot2 chart
#'
#' @param data a data to plot using ggplot2
#' @param xvar a variable to place in the aes() as x axis
#' @param yvar a variable to place in the aes() as y axis
#'
#' @return predefined form of graph
#' @export
#'
#' @examples
#' plotfn(trade_data, date, y_plot[i])
plotfn <- function(data, xvar, yvar){

  data_gd <- NULL

  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )

  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )

  ggplot(data = as.data.frame(data_gd),
         mapping = aes(x = xvar, y = yvar, lty = y_nms[i])
  ) +
    eval(parse(text = y_geom[i])) +
    scale_x_date(date_breaks = "1 month", date_labels = "%y/%m/%d") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.background = element_blank()) +
    annotate("text",
             x = min(data_gd$xvar),
             y = max(data_gd$yvar),
             hjust = 0.1,
             label = y_unit[i])
}

#' Create multi ggplot graphes (code origin - http://www.cookbook-r.com/Graphs)
#'
#' @return a multiplot
#' @export
#'
#' @examples
#' multiplot(p_1, p_2, p_3, p_4, cols = 2)
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
