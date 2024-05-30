#' Build a gauge with red, amber & green background
#'
#' @param pos Position of the arrow on the gauge, a value between 1-100
#' @param title Title for the individual gauge, defaults to blank
#' @param breaks Position of the breaks between colours on the dial
#'
#' @return Returns a ggplot object
#' @export
#'
#' @examples gg_gauge(52)
#' @examples gg_gauge(65, "How are we performing?")

gg_gauge <- function(pos,title = "", breaks=c(0,30,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot() +
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red", colour = "grey") +
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold", colour = "grey") +
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen", colour = "grey") +
    geom_polygon(data=get.poly({{pos}}-1,{{pos}}+1,0.2),aes(x,y)) +
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label= "")) +
        coord_fixed() +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10)) +
    labs(title = title)
}
