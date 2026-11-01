# ISPOR Short course on
# Health Economic Modeling in R for Decision Making
# Session 4 - Felicity Lamrock



## Probabilistic Sensitivity Analysis (PSA)

# There are n_samples defined globally in af_example_main.R
# The value is 1000 and we can see the dimensions in outputs
# The CE plane has 1000 results in a cloud shape.
# In the generate_input_parameters.R file there are n_samples for parameters
# Various distributions used such as uniform and normal distributions

# We can plot the input parameters here to see more clearly

# Uniform distribution
input_parameters[, "Coumarin cost"] <-
  runif(n_samples, 52.57, 157.70)

# Normal distribution
S_acute_cost_mean <- 11626
S_acute_cost_SE   <- 16868 / sqrt(162)

input_parameters[, "Stroke acute cost"] <-
  rnorm(n_samples, mean = S_acute_cost_mean,
        sd = S_acute_cost_SE)

# Fixed parameter
input_parameters[, "Apixaban cost"] <-
  rep(200.42, n_samples)

# Beta distribution
input_parameters[, "ICH utility"] <-
  pmin(rbeta(n_samples, 3.941, 1.385), 1) / 4


head(input_parameters[, c(
  "Coumarin cost",
  "Stroke acute cost",
  "Apixaban cost",
  "ICH utility"
)])


sd(input_parameters[, "Coumarin cost"])
sd(input_parameters[, "Stroke acute cost"])
sd(input_parameters[, "Apixaban cost"])
sd(input_parameters[, "ICH utility"])



## Scenario analysis

# Change age from 70 to 60

# We can change these values in af_example_main.R
initial_age <- 70
final_age   <- 100


# Base case
base_case <- generate_model_outputs(
  input_parameters,
  initial_age = 70,
  final_age = 100
)

# Scenario - younger starting age at age 60
scenario <- generate_model_outputs(
  input_parameters,
  initial_age = 60,
  final_age = 100
)

colMeans(base_case$total_costs)
colMeans(scenario$total_costs)

colMeans(base_case$total_qalys)
colMeans(scenario$total_qalys)

# Costs and QALYs both going up in the scenario with extra 10 years
# What about changing the model time horizon? 
# We could change final age to 90 to see any difference.
# There are no age specific hazards.




## Extreme value testing


# Hint look at generate_input_parameters.R line 68-71 to change the costs. You might want
# to not make changes in the orignal file but do something like this. 

# Create test inputs
test_inputs <- generate_input_parameters(n_samples)

# Set all treatment acquisition costs to zero
test_inputs[, "Coumarin cost"]   <- 0
test_inputs[, "Apixaban cost"]   <- 0
test_inputs[, "Dabigatran cost"] <- 0

# Calculate state costs when treatment costs are zero
zero_costs <- generate_state_costs(test_inputs)

# Now set Apixiban cost to £40,000 and compare the results. What is the difference?








## Unit testing


# Generate transition matrices
transition_matrix <- generate_transition_matrix(input_parameters)

# Look at one PSA sample and one treatment
round(
  transition_matrix[1, "Coumarin", , ],
  3
)

# Do the rows sum to 1?
rowSums(
  transition_matrix[1, "Coumarin", , ]
)

# We've manually found a problem in one transition matrix. 
# But our model contains a transition matrix for every PSA sample and treatment! 
# We can automate this check.
# Let's use testthat package

library(testthat)

test_that("2 + 2 equals 4", {
  expect_equal(2 + 2, 4)
})

test_that("2 + 2 equals 5", {
  expect_equal(2 + 2, 5)
})


