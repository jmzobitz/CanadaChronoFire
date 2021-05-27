#' From a listing of parameter estimates, filter on the rss between the 25th - 75h percentile, compute the median parameter estimates and the associated soil respiration
#'
#' \code{summarize_median_soil_respiration} summarizes the soil respiration from the median parameters
#'
#' @param input_results a vector of parameter estimates
#' @param input_expressions expressions we use to compute the model
#' @param input_expression data used to compute respiration
#' @param approach_name name of the approach for labeling
#'
#' @import dplyr


#' @export




summarize_median_soil_respiration <- function(input_results,input_expressions,input_data,approach_name) {

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
    nest()


  model_fn_inputs <-input_median_params %>%
    rename(params=data) %>%
    inner_join(input_expressions,by=c("model")) %>%
    inner_join(input_data,by=c("Year","depth"))


  # Compute the summary values and then we are ready to compute the taylor diagrams!
  model_values <- model_fn_inputs %>%
    mutate(outputs = pmap(.l=list(data,params,expressions),.f=~compute_model_respiration(..1,..2,..3)) ) %>%
    mutate(n_params = map(.x=params,.f=~(.x %>%  summarize(value = n()) %>% pull(value)))) %>%
    select(Year,depth,model,outputs,n_params) %>%
    mutate(approach = approach_name)


  return(model_values)


}


