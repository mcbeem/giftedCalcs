#' Plot method for class perf_by_cutoff
#'
#' @param x an object of class perf_by_cutoff to be plotted
#' @param colors a vector of two colors to be used in the plot for the sensitivity curve and cost line, respectively.
#'  Defaults to \code{c("#377eb8","#e41a1c")}
#' @param show.optimal logical; should the optimally cost-effective nomination cutoff be plotted? Defaults to TRUE
#' @param show.cost logical; should the proportional testing cost be plotted? defaults to TRUE
#'
#' @export

areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

plot.perf_by_cutoff <- function(x, colors=c("#377eb8","#e41a1c"),
                                show.optimal=TRUE, show.cost=TRUE) {

  # check inputs
  if (!is.logical(show.optimal)) {stop("Argument show.optimal must be TRUE or FALSE")}
  if (!is.logical(show.cost)) {stop("Argument show.cost must be TRUE or FALSE")}
  if (min(areColors(colors)) != 1) {stop("Argument colors must be a vector of two valid colors")}
  if (length(colors) != 2) {stop("Argument colors must be a vector of two valid colors")}

  wide <- x$perf_by_cutoff
  bestvalues <- x$optimal

  # perf_by_cutoff to long for ggplot
  parameter <- rep(c(1:2), each=nrow(wide))

  long <- cbind(
    rep(wide[,1], times=2),
    parameter,
    c(wide[,2], wide[,4])
  )

  long <- data.frame(long)
  names(long) <- c("nom.cutoff", "parameter", "value")
  long$parameter <- factor(long$parameter,
                           labels=c("sensitivity", "proportional cost"))

  if (show.cost==TRUE) {
  p <- ggplot2::ggplot(data=long, ggplot2::aes(x=nom.cutoff, y=value, col=parameter))+
      ggplot2::geom_line(alpha=.7)+
      ggplot2::theme_classic()+
      ggplot2::scale_color_manual(values=colors)+
      ggplot2::labs(x="Nomination cutoff (percentile)", y="Value")+
      ggplot2::scale_x_continuous(breaks=seq(0,1, .1))+
      ggplot2::scale_y_continuous(breaks=seq(0,1, .1))+
      ggplot2::coord_cartesian(xlim=c(0,1), ylim=c(0,1))
  }

  if (show.cost==FALSE) {
    p <- ggplot2::ggplot(data=long[long$parameter=="sensitivity",],
                         ggplot2::aes(x=nom.cutoff, y=value))+
      ggplot2::geom_line(alpha=.7, col=colors[1])+
      ggplot2::theme_classic()+
      ggplot2::scale_color_manual(values=colors)+
      ggplot2::labs(x="Nomination cutoff (percentile)", y="Sensitivity")+
      ggplot2::scale_x_continuous(breaks=seq(0,1, .1))+
      ggplot2::scale_y_continuous(breaks=seq(0,1, .1))+
      ggplot2::coord_cartesian(xlim=c(0,1), ylim=c(0,1))
  }

  if (show.optimal==TRUE & show.cost==TRUE) {
    p <- p +  ggplot2::geom_point(data=bestvalues,
                             ggplot2::aes(x=nom.cutoff, y=sensitivity), col="black", alpha=.9)+
      ggplot2::geom_vline(xintercept=bestvalues$nom.cutoff, alpha=.3)+
      ggplot2::geom_hline(yintercept=bestvalues$sensitivity,
                          alpha=.7, linetype="dashed", color=colors[1])+
      ggplot2::geom_hline(yintercept=bestvalues$cost,
                          alpha=.7, linetype="dashed", color=colors[2])
  }


  if (show.optimal==TRUE & show.cost==FALSE) {
    p <- p +  ggplot2::geom_point(data=bestvalues,
                                  ggplot2::aes(x=nom.cutoff, y=sensitivity), col="black", alpha=.9)+
      ggplot2::geom_vline(xintercept=bestvalues$nom.cutoff, alpha=.3)+
      ggplot2::geom_hline(yintercept=bestvalues$sensitivity,
                          alpha=.7, linetype="dashed", color=colors[1])
  }
  return(p)
}

