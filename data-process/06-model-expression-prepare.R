# Prepare the model expressions into a data frame
library(tidyverse)

# We need a list of model expressions:
# Define the model expressions globally because they are used.

# 9/21: Replaced fW with 3.11fW-2.42fW^2 for some limitation of anoxic conditions (Moyano)

# Incubation model
null_model_expr <- respiration ~ kS*CS*Q10M^((temperature)/10)
microbe_model_expr <- respiration ~ mu*epsilon*CS/(kA+CS)*CM+kM*CM*Q10M^((temperature)/10)
quality_model_expr <- respiration ~ mu*epsilon*CA/(kA+CA)*CM+kM*CM*Q10M^((temperature)/10)
microbe_mult_model_expr <-  respiration ~ CS/(kA+CS)*kM*CM*Q10M^((temperature)/10)
quality_mult_model_expr <- respiration ~ CA/(kA+CA)*kM*CM*Q10M^((temperature)/10)

# Field Model (also works to compute respiration for field and incubation field stats)
field_null_model_expr <- respiration ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10) + (3.11*fW-2.42*fW^2)*kS*CS*Q10M^((temperature)/10)
field_microbe_model_expr <- respiration ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10) + (mu*epsilon*CS/(kA+CS)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
field_quality_model_expr <- respiration ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10) + (mu*epsilon*CA/(kA+CA)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
field_microbe_mult_model_expr <-  respiration ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10) + ((3.11*fW-2.42*fW^2)*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
field_quality_mult_model_expr <- respiration ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10) +((3.11*fW-2.42*fW^2)*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))

# Field Linear
field_linear_null_model_expr <- respiration ~ gR*CR + (3.11*fW-2.42*fW^2)*kS*CS*Q10M^((temperature)/10)
field_linear_microbe_model_expr <- respiration ~ gR*CR + (mu*epsilon*CS/(kA+CS)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
field_linear_quality_model_expr <- respiration ~ gR*CR + (mu*epsilon*CA/(kA+CA)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
field_linear_microbe_mult_model_expr <-  respiration ~ gR*CR + ((3.11*fW-2.42*fW^2)*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
field_linear_quality_mult_model_expr <- respiration ~ gR*CR + ((3.11*fW-2.42*fW^2)*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))

# Incubation Field Linear
rs_incubation_field_linear_null_model_expr <- respiration ~ gR*CR + f*(3.11*fW-2.42*fW^2)*kS*CS*Q10M^((temperature)/10)
rs_incubation_field_linear_microbe_model_expr <- respiration ~ gR*CR + f*(mu*epsilon*CS/(kA+CS)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
rs_incubation_field_linear_quality_model_expr <- respiration ~ gR*CR + f*(mu*epsilon*CA/(kA+CA)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
rs_incubation_field_linear_microbe_mult_model_expr <-  respiration ~ gR*CR + f*((3.11*fW-2.42*fW^2)*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
rs_incubation_field_linear_quality_mult_model_expr <- respiration ~ gR*CR + f*((3.11*fW-2.42*fW^2)*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))

ra_field_null_model_expr <- RA ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10)
ra_field_microbe_model_expr <- RA ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10)
ra_field_quality_model_expr <- RA ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10)
ra_field_microbe_mult_model_expr <-  RA ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10)
ra_field_quality_mult_model_expr <- RA ~ (3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10)


rh_field_null_model_expr <- RH ~  (3.11*fW-2.42*fW^2)*kS*CS*Q10M^((temperature)/10)
rh_field_microbe_model_expr <- RH ~  (mu*epsilon*CS/(kA+CS)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
rh_field_quality_model_expr <- RH ~  (mu*epsilon*CA/(kA+CA)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
rh_field_microbe_mult_model_expr <-  RH ~  ((3.11*fW-2.42*fW^2)*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
rh_field_quality_mult_model_expr <- RH ~ ((3.11*fW-2.42*fW^2)*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))

rh_incubation_field_linear_null_model_expr <- RH ~  f*(3.11*fW-2.42*fW^2)*kS*CS*Q10M^((temperature)/10)
rh_incubation_field_linear_microbe_model_expr <- RH ~  f*(mu*epsilon*CS/(kA+CS)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
rh_incubation_field_linear_quality_model_expr <- RH ~  f*(mu*epsilon*CA/(kA+CA)*CM+(3.11*fW-2.42*fW^2)*kM*CM*Q10M^((temperature)/10))
rh_incubation_field_linear_microbe_mult_model_expr <-  RH ~  f*((3.11*fW-2.42*fW^2)*CS/(kA+CS)*kM*CM*Q10M^((temperature)/10))
rh_incubation_field_linear_quality_mult_model_expr <- RH ~ f*((3.11*fW-2.42*fW^2)*CA/(kA+CA)*kM*CM*Q10M^((temperature)/10))


# Collect all of these expressions into a data frame that we can use
respiration_expressions <-
  tibble( model = c("null","microbe","quality","microbe-mult","quality-mult"),
          RA_expression = c(ra_field_null_model_expr,  # For null and field
                            ra_field_microbe_model_expr,
                            ra_field_quality_model_expr,
                            ra_field_microbe_mult_model_expr,
                            ra_field_quality_mult_model_expr),
          RH_expression = c(rh_field_null_model_expr, # For null, field, and incubation field
                            rh_field_microbe_model_expr,
                            rh_field_quality_model_expr,
                            rh_field_microbe_mult_model_expr,
                            rh_field_quality_mult_model_expr),
          RA_linear_expression = c(RA ~ gR*CR),  # For field-linear and incubation-field-linear
          RH_incubation_field_linear_expression = c(rh_incubation_field_linear_null_model_expr, # for incubation field linear
                                                    rh_incubation_field_linear_microbe_model_expr,
                                                    rh_incubation_field_linear_quality_model_expr,
                                                    rh_incubation_field_linear_microbe_mult_model_expr,
                                                    rh_incubation_field_linear_quality_mult_model_expr),
          rSoil_expression = c(field_null_model_expr,  # Works for field and incubation field
                               field_microbe_model_expr,
                               field_quality_model_expr,
                               field_microbe_mult_model_expr,
                               field_quality_mult_model_expr),
          rSoil_incubation_field_linear_expression = c(rs_incubation_field_linear_null_model_expr,
                                                       rs_incubation_field_linear_microbe_model_expr,
                                                       rs_incubation_field_linear_quality_model_expr,
                                                       rs_incubation_field_linear_microbe_mult_model_expr,
                                                       rs_incubation_field_linear_quality_mult_model_expr),
          rSoil_field_linear_expression = c(field_linear_null_model_expr,
                                            field_linear_microbe_model_expr,
                                            field_linear_quality_model_expr,
                                            field_linear_microbe_mult_model_expr,
                                            field_linear_quality_mult_model_expr),
  )


model_expressions <- tibble( model = c("null","microbe","quality","microbe-mult","quality-mult"),
                             incubation_expressions = c(null_model_expr,microbe_model_expr,quality_model_expr,microbe_mult_model_expr,quality_mult_model_expr),
                             field_expressions = c(field_null_model_expr, field_microbe_model_expr, field_quality_model_expr, field_microbe_mult_model_expr, field_quality_mult_model_expr),
                             field_linear_expressions = c(field_linear_null_model_expr, field_linear_microbe_model_expr, field_linear_quality_model_expr, field_linear_microbe_mult_model_expr, field_linear_quality_mult_model_expr),
                             incubation_field_expressions = c(respiration~(3.11*fW-2.42*fW^2)*kR*CR*Q10R^((temperature)/10)+rH),
                             incubation_field_linear_expressions = c(respiration ~ gR*CR+ f*rH))

# Save these as datasets we can utilize

usethis::use_data(model_expressions,overwrite = TRUE)
usethis::use_data(respiration_expressions,overwrite = TRUE)
