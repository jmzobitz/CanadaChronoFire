#' Compute modeled soil respiration components from parameters and data
#'
#' \code{compute_model_respiration_components} Determines modeled soil respiration given data, parameters, and an expression
#'
#' @param input_data Data used to compute respiration
#' @param input_paramms Parameter values used in expression
#' @param input_ra_exp Model expression used to compute RA
#' @param input_rh_exp Model expression used to compute RH
#'
#' @export
#'
#'
compute_model_respiration_components <- function(input_data,input_params,input_ra_exp,input_rh_exp) {



  my_params <- input_params  %>% deframe()

  with(as.list(c(my_params)), {


    rA_model <- eval(formula.tools::rhs(input_ra_exp),envir=input_data) %>% as.vector()

    rH_model <- eval(formula.tools::rhs(input_rh_exp),envir=input_data) %>% as.vector()

    rA_p_model <- rA_model / (rH_model + rA_model)

    return(tibble(rA = rA_model, rH = rH_model,rA_prop = rA_p_model,rS = rA+rH))

  })


}
