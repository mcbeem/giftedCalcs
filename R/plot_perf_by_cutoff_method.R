#' Plot method for class perf_by_cutoff
#'
#' @param x an object of class perf_by_cutoff to be plotted
#' @param colors a vector of two colors to be used in the plot for the sensitivity curve and cost line, respectively.
#'  Defaults to \code{c("#377eb8","#e41a1c")}
#'
#' @export

plot.perf_by_cutoff <- function(x, colors=c("#377eb8","#e41a1c")) {

  wide <- x$perf_by_cutoff

  # perf_by_cutoff to long for ggplot
  parameter <- rep(c(1:2), each=length(nom.cutoff))

  long <- cbind(
    rep(nom.cutoff, times=2),
    parameter,
    c(perf_by_cutoff[,2], perf_by_cutoff[,4])
  )

  long <- data.frame(long)
  names(long) <- c("nom.cutoff", "parameter", "value")
  long$parameter <- factor(long$parameter,
                           labels=c("sensitivity", "proportional cost"))

  p <- ggplot2::ggplot(data=long, ggplot2::aes(x=nom.cutoff, y=value, col=parameter))+
    ggplot2::geom_line(alpha=.7)+
    ggplot2::geom_point(data=long[bestindex,],
                        ggplot2::aes(x=nom.cutoff, y=value), col="black", alpha=.9)+
    ggplot2::geom_vline(xintercept=as.numeric(long[bestindex,1]),
               alpha=.5, linetype="dashed")+
    ggplot2::geom_hline(yintercept=as.numeric(long[bestindex+length(nom.cutoff),3]),
               alpha=.7, linetype="dashed", color=colors[2])+
    ggplot2::geom_hline(yintercept=as.numeric(long[bestindex,3]),
               alpha=.7, linetype="dashed", color=colors[1])+
    ggplot2::theme_classic()+
    ggplot2::scale_color_manual(values=colors)+
    ggplot2::labs(x="Nomination cutoff (percentile)", y="Value")+
    ggplot2::scale_x_continuous(breaks=seq(0,1, .1))+
    ggplot2::scale_y_continuous(breaks=seq(0,1, .1))

  return(p)
}

