#' Compute modeled soil respiration from parameters and data
#'
#' \code{compute_model_respiration Determines modeled soil respiration given data, parameters, and an expression
#'
#' @param input_data Data used to compute respiration
#' @param input_paramms Parameter values used in expression
#' @param input_model_exp Model expression used to compute respiration
#'
#' @export
#'
#'
compute_model_respiration <- function(input_data,input_params,input_model_exp) {

  params <- input_params %>% deframe()

  with(as.list(c(input_data,params)), {
    rSoil_model <- eval(formula.tools::rhs(input_model_exp),envir = input_data)

    return(tibble(rSoil = input_data$respiration, rModel = rSoil_model))
  })


}
