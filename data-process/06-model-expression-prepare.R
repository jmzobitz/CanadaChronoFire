# Prepare the model expressions into a data frame
library(tidyverse)

# We need a list of model expressions:
# Define the model expressions globally because they are used.
null_model_expr <- respiration ~ kS*CS*Q10M^((temperature)/10)
microbe_model_expr <- respiration ~ mu*epsilon*CS/(kA+CS)*CM+kM*CM*Q10M^((temperature)/10)
quality_model_expr <- respiration ~ mu*epsilon*CA/(kA+CA)*CM+kM*CM*Q10M^((temperature)/10)
microbe_mult_model_expr <-  respiration ~ CS/(kA+CS)*kM*CM*Q10M^((temperature)/10)
quality_mult_model_expr <- respiration ~ CA/(kA+CA)*kM*CM*Q10M^((temperature)/10)

field_null_model_expr <- respiration ~ fW*kR*CR*Q10R^((temperature)/10) + fW*kS*CS*Q10M^((temperature)/10)
field_microbe_model_expr <- respiration ~ fW*kR*CR*Q10R^((temperature)/10) + (mu*epsilon*CS/(kA+CS)*CM+fW*kM*CM*Q10M^((temperature)/10))
field_quality_model_expr <- respiration ~ fW*kR*CR*Q10R^((temperature)/10) + (mu*epsilon*CA/(kA+CA)*CM+fW*kM*CM*Q10M^((temperature)/10))
field_microbe_mult_model_expr <-  respiration ~ fW*kR*CR*Q10R^((temperature)/10) + (fW*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
field_quality_mult_model_expr <- respiration ~ fW*kR*CR*Q10R^((temperature)/10) +(fW*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))


rev_field_null_model_expr <- respiration ~ rA*CR + fW*kS*CS*Q10M^((temperature)/10)
rev_field_microbe_model_expr <- respiration ~ rA*CR + (mu*epsilon*CS/(kA+CS)*CM+fW*kM*CM*Q10M^((temperature)/10))
rev_field_quality_model_expr <- respiration ~ rA*CR + (mu*epsilon*CA/(kA+CA)*CM+fW*kM*CM*Q10M^((temperature)/10))
rev_field_microbe_mult_model_expr <-  respiration ~ rA*CR + (fW*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
rev_field_quality_mult_model_expr <- respiration ~ rA*CR +(fW*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))



ra_field_null_model_expr <- RA ~ fW*kR*CR*Q10R^((temperature)/10)
ra_field_microbe_model_expr <- RA ~ fW*kR*CR*Q10R^((temperature)/10)
ra_field_quality_model_expr <- RA ~ fW*kR*CR*Q10R^((temperature)/10)
ra_field_microbe_mult_model_expr <-  RA ~ fW*kR*CR*Q10R^((temperature)/10)
ra_field_quality_mult_model_expr <- RA ~ fW*kR*CR*Q10R^((temperature)/10)


rh_field_null_model_expr <- RH ~  fW*kS*CS*Q10M^((temperature)/10)
rh_field_microbe_model_expr <- RH ~  (mu*epsilon*CS/(kA+CS)*CM+fW*kM*CM*Q10M^((temperature)/10))
rh_field_quality_model_expr <- RH ~  (mu*epsilon*CA/(kA+CA)*CM+fW*kM*CM*Q10M^((temperature)/10))
rh_field_microbe_mult_model_expr <-  RH ~  (fW*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
rh_field_quality_mult_model_expr <- RH ~ (fW*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))

# Collect all of these expressions into a data frame that we can use
respiration_expressions <- tibble( model = c("null","microbe","quality","microbe-mult","quality-mult"),
                                   RA_expression = c(ra_field_null_model_expr, ra_field_microbe_model_expr, ra_field_quality_model_expr, ra_field_microbe_mult_model_expr, ra_field_quality_mult_model_expr),
                                   RH_expression = c(rh_field_null_model_expr, rh_field_microbe_model_expr, rh_field_quality_model_expr, rh_field_microbe_mult_model_expr, rh_field_quality_mult_model_expr) )


model_expressions <- tibble( model = c("null","microbe","quality","microbe-mult","quality-mult"),
                             incubation_expressions = c(null_model_expr,microbe_model_expr,quality_model_expr,microbe_mult_model_expr,quality_mult_model_expr),
                             field_expressions = c(field_null_model_expr, field_microbe_model_expr, field_quality_model_expr, field_microbe_mult_model_expr, field_quality_mult_model_expr),
                             rev_field_expressions = c(rev_field_null_model_expr, rev_field_microbe_model_expr, rev_field_quality_model_expr, rev_field_microbe_mult_model_expr, rev_field_quality_mult_model_expr))

# Save these as datasets we can utilize

use_data(model_expressions,overwrite = TRUE)
use_data(respiration_expressions,overwrite = TRUE)

