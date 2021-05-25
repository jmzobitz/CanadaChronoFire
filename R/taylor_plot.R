# Just a function the returns a Taylor plot geom - easier for use to move the code into here

#' Make a default Taylor diagram for use
#'
#' \code{taylor_plot} Generates a ggplot structure to make a Taylor diagram
#'
#' @param min_R minimum radius of concentric circles
#' @param max_R max radius of concentric circles
#' @param contours number of concentric circle contours between min_R and max_R
#' @param n_lines number of angle contours that measure correlation coeff.
#' @param x_0 location of data reference on horizontal axis
#' @param ref_r_min minimum radius of reference circles from data reference point
#' @param ref_r_max maximum radius of reference circles from data reference point
#' @param ref_contours number of concentric circle contours between ref_r_min and ref_r_max
#' @param full TRUE or FALSE.  TRUE = plots a half circle, FALSE = plots a quarter circle

#' @source Taylor, Karl E. 2001. “Summarizing Multiple Aspects of Model Performance in a Single Diagram.” Journal of Geophysical Research: Atmospheres 106 (D7): 7183–92. https://doi.org/10.1029/2000JD900719.

#' @return A plot default plotting structure of a circle in the first quadrant.
#' @examples
#'
#' taylor_plot()

#' @import dplyr
#' @import ggplot2
#' @import ggforce
#' @export



taylor_plot <- function(min_R = 0.25, max_R = 1.75, contours = 7, n_lines = 10,x_0 = 1, ref_r_min=0.25, ref_r_max=2,ref_contours = 8,full=FALSE,y_axis_label='\u03C3') {

  # Behold the arcs
  arcs <- data.frame(
    start = array(0,dim=contours),
    end = array(pi/2,dim=contours),
    r = seq(min_R,max_R,length.out=contours)
  )

  if(full) {
    corr_start <- -1
  } else {
    corr_start <- 0
  }

  # Define the correlation angles
  rSeq = seq(corr_start,1,length.out=n_lines)
  angle=acos(rSeq)
  seg_data = data.frame(x1=0,y1=0,x2=max_R*cos(angle),y2=max_R*sin(angle),rSeq)


  ## Now we need to figure out the starting and ending angles for this
  # Intersection with outer circle:


  ref_r = seq(ref_r_min,ref_r_max,length.out = ref_contours)

  x_star = array(0,dim=ref_contours)
  x_star <- (ref_r^2 - max_R^2 - x_0^2)/(-2*x_0) # Null case
  x_star[ref_r < (max_R-x_0) ] = x_0+ref_r[ref_r < (max_R-x_0) ] # Case when ref_r isn't larger than the circle
  #x_star[x_star<0]=NaN  # If x_star < 0, it doesn't intersect in positive quadrant


  # To find the angle, we want to test two cases: x_star < x_0, x_star>x_0


  # Now determine the y points where we intersect
  y_star=sqrt(ref_r^2-(x_star-x_0)^2)
  y_star[ref_r < (max_R-x_0)]=0 # Case when ref_r isn't larger than the circle

  # The angle we end at depends on the location where x_star is
  angle_end=pi/2-atan(y_star/((x_star-x_0)))  # default location x_star > x_0
  angle_end[x_star < x_0] <- -pi/2+atan(y_star[x_star < x_0]/(x_0-x_star[x_star < x_0]))


  if(full) {
    angle_begin <- -pi/2
  } else {
    y_left <- sqrt(ref_r^2-x_0^2)  # Place where x = 0.

    angle_begin <- -pi/2+atan(y_left)
    angle_begin[is.nan(angle_begin)] = -pi/2
  }



  small_arcs <- data.frame(x_star,y_star,angle_begin,angle_end,ref_r) %>%
    filter(x_star>0)

  if(full) {
    arc_begin = -pi/2
    R_xlim <- -max_R}
  else {
    arc_begin = 0
    R_xlim <- 0}
  # Add the circles with the contours + text
  taylor_figure <- ggplot() + ggforce::geom_arc(aes(x0=0, y0=0, r=r, start=arc_begin, end=pi/2),
                                     data=arcs)  +
   # coord_fixed() +
    coord_cartesian(xlim=c(R_xlim,max_R),ylim=c(0,1.05*max_R)) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = seg_data,color="grey") +
    geom_text(data=seg_data,aes(x=1.05*x2,y=1.05*y2,label=round(rSeq,1)),size=2) +
    ggforce::geom_arc(aes(x0=x_0, y0=0, r=ref_r, start=angle_begin, end=angle_end),
             data=small_arcs,linetype=2) +
    geom_point(data=data.frame(x=x_0,y=0),aes(x=x,y=y),size=2,color='red',shape=15) +
    scale_x_continuous(name="") +
    ylab(y_axis_label)



  return(taylor_figure)
}











