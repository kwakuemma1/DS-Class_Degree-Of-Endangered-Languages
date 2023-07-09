library(tidyverse) # for data manipulation
library(rstanarm) 

# combined data is created from sourcing the file 'data_01_clean_languages_final.R'
# source('data_01_clean_languages_final.R')
the_data <- readRDS('combined_data.Rds')

the_targets = c('degree_of_endangerment_numeric', 'degree_of_endangerment_factor')

covariates <- c(
  'literacy',
  'infant_mortality',
  'agriculture',
  'proximity_to_capital_city',
  'minority_ed_policy',
  'urban_pop_pc_change_1960_2017'
)


# Keep only columns needed to build the models
the_data <- the_data |>
  dplyr::select(any_of(c(covariates, the_targets)))


# Model A: Predict Numeric Degree of Endangerment -------------------------

model_formula_numeric <- as.formula(
  paste0(
    'degree_of_endangerment_numeric ~ ', 
    paste0(covariates, collapse = '+')
  )
)


# Predicting the degree of endangerment as a number
model_numeric <- stan_glm(data = the_data, 
                          formula = model_formula_numeric, 
                          family = gaussian, 
                          refresh = 0,
                          seed = 9)

saveRDS(model_numeric, "model_numeric.Rds")



# Model B: Predict Probability of Degree of Endangerment ------------------

model_formula_factor <- as.formula(
  paste0(
    'degree_of_endangerment_factor ~ ', 
    paste0(covariates, collapse = '+')
  )
)

# Predicting the PROBABILITY of endangerment of each of the degrees of endangerment
model_factor <- stan_polr(data = the_data, 
                          formula = model_formula_factor, 
                          method = "logistic",
                          prior = R2(0.25),
                          refresh = 0,
                          seed = 9)

saveRDS(model_factor, "model_factor.Rds")



# Equations Print Out -----------------------------------------------------

library(equatiomatic)

frequentist_numeric_equivalent <- lm(model_formula_numeric, data=the_data)
extract_eq(frequentist_numeric_equivalent,terms_per_line = 2, wrap = TRUE, operator_location = "start")

extract_eq(frequentist_numeric_equivalent,terms_per_line = 2, 
           wrap = TRUE, operator_location = "start", use_coefs = TRUE)


frequentist_factor_equivalent <- MASS::polr(model_formula_factor, data=the_data, method = "logistic")
extract_eq(frequentist_factor_equivalent,terms_per_line = 2, wrap = TRUE, operator_location = "start")

extract_eq(frequentist_factor_equivalent,terms_per_line = 2, 
           wrap = TRUE, operator_location = "start", use_coefs = TRUE)


