#' Parameter estimate using the Nash equivalent of LM algorithm.
#'
#' \code{parameter_estimate} Determines a parameter estimate from input data.  I had to modify if from nlxb so it doesn't print out the progress messages.
#'
#' @param parameters a vector of parameters we use to estimate data
#' @param input_data data we use for the model
#' @param input_expression  the point expression we use for the model
#'
#' @import dplyr


#' @export



parameter_estimate <- function(parameters,input_data,input_expression) {

  ## lower and upper values
  lower_val <- parameters %>%
    mutate(min_value = if_else(estimate,min_value,value)) %>% # If we are estimating the parameter we set it to the min value, otherwise the stated value.
    select(name,min_value) %>%
    tibble::deframe()

  upper_val <- parameters %>%
    mutate(max_value = if_else(estimate,max_value,value)) %>% # If we are estimating the parameter we set it to the max value, otherwise the stated value.
    select(name,max_value) %>%
    tibble::deframe()

  # Identify the initial guess
  guess <-
    parameters %>%
    rowwise() %>%
    mutate(value=if_else(estimate,stats::runif(1,min=min_value,max=max_value),value)) %>%  # If we estimate this we randomly select, otherwise keep the stated value
    select(name,value) %>%
    tibble::deframe()

  # Identify the "fixed" (not estimated parameters)
  fixed_param_name <- parameters %>%
    filter(!estimate) %>% select(name) %>% tibble::deframe()

  # OK!  We should be able to do this now

  out <- nlxb_jz(input_expression,
                 data = input_data,
                 start = guess,
                 lower = lower_val,
                 upper = upper_val,
                 weights = stats::var(input_data$respiration),
                 masked = fixed_param_name,
                 control=list(watch=FALSE),
                 trace=FALSE)


  return(out)



}
