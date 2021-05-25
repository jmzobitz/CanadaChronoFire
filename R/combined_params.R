#' Combined parameter set for data assimilation. This helps to have a common dataset used across all methods.
#'
#' \itemize{
#'   \item model. Type of model used - categorical variable (one of "null", "microbe", "quality", "microbe-mult", "quality-mult")
#'   \item depth. Categorical variable explaining the depth of measurement (5 cm, 10 cm, or ALL (both depths combined))
#'   \item incubation. incubation model parameters for that model
#'   \item field. field model parameters for that model
#'   \item model_estimate.  boolean vector if the parameter is in the model (for any part) - helps for subsequent analysis and visualization
#' }
#'
#'
#' @docType data
#' @keywords datasets
#' @name combined_params
#' @usage data(combined_params)
#' @format A data frame with 15 rows, 4 columns.
#' @source Expressions processed and exported in data-process/02-parameter-prepare.R

NULL
