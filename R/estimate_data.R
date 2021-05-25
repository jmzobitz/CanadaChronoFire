#' Combined parameter and data set for data assimilation. This helps to have a common dataset used across all methods.
#'
#' \itemize{
#'   \item Year. Categorical variable explaining the site (one of "N2012", "N1990", "NC", "N1968", "NALL")
#'   \item depth. Categorical variable explaining the depth of measurement (5 cm, 10 cm, or ALL (both depths combined))
#'   \item incubation_data. nested incubation data for assimilation
#'   \item field_data. nested field data for assimilation
#'   \item model. Type of model used - categorical variable (one of "null", "microbe", "quality", "microbe-mult", "quality-mult")
#'   \item incubation_params. incubation model parameters for that model
#'   \item field_params. field model parameters for that model (assuming incubation data estimated first)
#'   \item model_estimate.  boolean vector if the parameter is in the model (for any part) - helps for subsequent analysis and visualization
#'   \item incubation_expressions. The formula needed to compute soil respiration from incubation data
#'   \item field_expressions  The formula needed to compute soil respiration from field data
#'   \item rev_field_expressions. A modified formula for the Field Linear and Incubation Field Linear approaches
#' }
#'
#'
#' @docType data
#' @keywords datasets
#' @name estimate_data
#' @usage data(estimate_data)
#' @format A data frame with 75 rows, 11 columns.
#' @source Expressions processed and exported in data-process/07-estimate-prepare.R

NULL
