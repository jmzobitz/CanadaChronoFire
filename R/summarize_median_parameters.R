#' From a listing of parameter estimates, filter on the rss between the 25th - 75h percentile, compute the median parameter estimates and the associated soil respiration
#'
#' \code{summarize_median_parameters} returns the median parameters from a nested parameter estimations
#'
#' @param input_results a nested (by Year, model, depth) vector of parameter estimates
#' @param approach_name name of the approach for labeling
#'
#' @import dplyr


#' @export




summarize_median_parameters <- function(input_results,approach_name) {

  # Compute the parameter summary
  input_parameter_summary <- input_results %>%
    mutate(summary_param = map(.x=data, .f=~(.x$params %>%
                                               bind_rows() %>%
                                               group_by(name) %>%
                                               nest() %>%
                                               mutate(fivenum = map(.x=data,.f=~(summary(.x$value) %>% enframe()) )   ) %>%
                                               mutate(conf_int =  map(.x=data,.f=~(tibble(ci_value=quantile(.x$value, c(0.025, 0.5, 0.975)), quantile = c("q0.025", "q0.5", "q0.975")))  ) )))) %>% select(Year,depth,model,summary_param) %>%
    unnest(cols=summary_param) %>%
    select(-data) %>% group_by(Year,depth,model) %>%
    nest()


  # Compute the median parameter values
  input_median_params <- input_parameter_summary %>%
    unnest(col=c(data)) %>%
    unnest(col=conf_int) %>%
    filter(quantile=="q0.5") %>%
    select(Year,depth,model,name,ci_value) %>%
    group_by(Year,depth,model) %>%
    nest() %>%
    mutate(approach=approach_name)



  return(input_median_params)


}


