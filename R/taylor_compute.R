#' Function that just computes Taylor stats for the model and the data
#'
#' \code{taylor_compute} computes all necessary information to produce a Taylor diagram
#' @param modeled Vector of modeled values
#' @param measured Vector of measured values

#' @return A data frame of results: sd_meas, sd_model (standard deviations of measured and modeled), r = correlation coefficient, centered root mean squared, and x and y coordinates for a taylor diagram)

#' @export
#'
taylor_compute <- function(modeled,measured) {

  my_tibble <- tibble(modeled,measured) %>% na.omit()
  modeled = my_tibble$modeled
  measured = my_tibble$measured

  out_values <- tibble(
    sd_meas = 1,
    sd_model = sd(modeled) / sd(measured),
    r = cor(modeled,measured),
    centered_rms = sd((measured-mean(measured))-((modeled-mean(modeled))))/sd(measured),
    x_coord = sd_model*r,
    y_coord = sd_model*sin(acos(r))

  )
  return(out_values)
}
