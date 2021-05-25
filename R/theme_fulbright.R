#' Define a custom theme for making ggplots
#'
#' \code{theme_fulbright} Sets up defaults for ggplots so I don't need to keep doing these over and over.
#'

theme_fulbright <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
          axis.text = ggplot2::element_text(size=14),
          axis.title=ggplot2::element_text(size=28),
          title=ggplot2::element_text(size=26),
          legend.text=ggplot2::element_text(size=12),
          legend.title=ggplot2::element_text(size=14),
          strip.text.x = ggplot2::element_text(size=12),
          strip.text.y = ggplot2::element_text(size=12),
          strip.background = ggplot2::element_rect(colour="white", fill="white")) +
    ggplot2::theme( panel.grid.major = ggplot2::element_blank(),
           panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

}
