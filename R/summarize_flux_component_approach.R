#' \code{summarize_flux_component_approach} Determines median soil respiration components for a given model approachodeled soil respiration given data, parameters, and an expression
#'
#' @param input_results Nested data frame (by Year, depth, model) of parameter estimates
#' @param input_resp_exp Input respiration expressions used to compute. Should be a data frame with columns: (1) model, RA_expression, RH_expression
#' @param approach_name Name of estimation approach (usually one of Field, Field Linear, etc)
#'
#' @export
#'
#'

summarize_flux_component_approach <- function (in_results,in_resp_expr,approach_name) {


  my_inputs <- in_results %>%
    inner_join(combined_data,by=c("Year","depth")) %>%
    select(-incubation) %>%
    unnest(cols=c(data)) %>%
    inner_join(in_resp_expr,by="model")

  flux_components <- my_inputs %>%
    mutate(fluxes = pmap(list(field,params,RA_expression,RH_expression),
                         .f=~compute_model_flux_components(..1,..2,..3,..4))
    ) %>%
    select(Year,depth,model,fluxes)

  # Then compute the median across the fluxes
  median_results <- flux_components %>%
    select(Year,depth,model,fluxes) %>%
    unnest(cols=c(fluxes)) %>%
    select(-rS) %>%
    group_by(Year,depth,model) %>%
    summarize(across(.cols=c("rA","rH","rA_prop"),.fns=median)) %>%
    ungroup() %>%
    mutate(approach = approach_name)

  return(median_results)


}
