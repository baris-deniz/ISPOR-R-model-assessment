# Example Atrial Fibrillation (AF) Model for ISPOR Short course on
# Health Economic Modeling in R for Decision Making
# Assessment, Adaptation, and Al-Assisted Validation
# Howard Thom with Felicity Lamrock, Eline Krijkamp, Baris Deniz

# Based on AF Teaching example from Economic Evaluation Modelling short course
# at University of Bristol medical school
# Itself based on the AF model and network meta-analysis (NMA) described in
# https://www.bmj.com/content/bmj/359/bmj.j5058.full.pdf
# https://pmc.ncbi.nlm.nih.gov/articles/PMC6699015/
# This model was later adapted for the NICE 2021 Guidelines on AF

# Script conduct cost-effectiveness analysis of atrial fibrillation using example Markov model


# Load BCEA library to help analyse and visualise results
library(BCEA)

set.seed(14142234)

# Load the necessary modules
source("generate_input_parameters.R")
source("generate_hazards.R")
source("generate_transition_matrix.R")
source("generate_state_utilities.R")
source("generate_state_costs.R")
source("generate_model_outputs.R")


# Define global simulation parameters
n_samples <- 1000

# Define global model structure parameters
state_names <- c("AF Well", "Stroke", "ICH", "MI", "Bleed", "Dead")
n_states    <- length(state_names)

treatment_names <- c("Coumarin", "Apixaban", "Dabigatran")
n_treatments    <- length(treatment_names)

event_names <- c("Stroke", "MI", "Bleed", "ICH", "Death", "SE", "TIA")
n_events    <- length(event_names)

# Define global scenario parameters
initial_age <- 70
final_age   <- 100

n_wtp_UK    <- 25000 # add willingness to pay threshold 

# Generate the input parameters
# This will be converted into transition matrix, state costs, and state utilities
input_parameters <- generate_input_parameters(n_samples = n_samples)

# Run the Markov model to get the model outputs
model_outputs <- generate_model_outputs(input_parameters, 
                                        initial_age = initial_age, 
                                        final_age   = final_age)

####################################################################################################
## Quick manual check of results ###################################################################
####################################################################################################


with(model_outputs, colMeans(total_qalys))
with(model_outputs, colMeans(total_costs))
# Expected net benefit at 25,000
with(model_outputs, colMeans(n_wtp_UK * total_qalys - total_costs))


                    
##################################################################################################################
## Now use BCEA to analyse the outputs   #########################################################################
##################################################################################################################
# Create a bcea object for the doac model
doac_bcea <- bcea(e = model_outputs$total_qalys, 
                  c = model_outputs$total_costs, 
                  ref = 1, 
                  interventions = treatment_names)

# Negative ICER means one strategy dominates and the ICER is not informative 
# and should therefore not be calculated or shown
# One should look to the inc. costs and inc. effects to decide on the dominant strategy
doac_bcea$ICER[doac_bcea$ICER < 0] <- NA

# Summarise the results
summary(doac_bcea, wtp = n_wtp_UK)

# Plot cost-effectiveness plane using base graphics
# Replace the internal ICER label with empty text to avoid printing ICER (base graphics only)
assignInNamespace("icer_label", function(he) "", ns = "BCEA")

# Coumarin vs apixaban, with manually tweaked x (£cost) limits
ceplane.plot(doac_bcea, 
             comparison = 2, 
             wtp = n_wtp_UK, 
             graph = "ggplot2", 
             xlim = c(-150, 150),
             title = "Cost-effectiveness plane Coumarin vs Apixaban")

# Coumarin vs dabigatran, with manually tweaked x (£cost) limits
ceplane.plot(doac_bcea, 
             comparison = 3, 
             wtp = n_wtp_UK, 
             graph = "base", 
             xlim = c(-500, 500),
             title = "Cost-effectiveness plane Coumarin vs Dabigatran") 

# Compare all three treatments 
# For multiple treatment comparison
doac_multi_ce <- multi.ce(doac_bcea)
# Cost-effectiveness acceptability curve
ceac.plot(doac_multi_ce, graph = "ggplot",
          line = list(color = c("red", "green", "blue")),
          pos = c(0, 0.50))
