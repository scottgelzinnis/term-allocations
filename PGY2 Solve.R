#Set WD
setwd("C:/Users/Scotty/Desktop/Allocations Linear Optimisation")

# Install and load the necessary package
install.packages("randomNames")
install.packages("lpSolve")
install.packages("lpSolveAPI")
install.packages("ROI")
install.packages("knitr")
install.packages("gt")

# Load necessary libraries
library(readr)
library(tidyverse)
library(skimr)
library(lpSolve)
library(randomNames)
library(lpSolveAPI)
library(ROI)
library(knitr)
library(gt)


# Random seed
set.seed(69)

## Define preference matrix  ##

# Set the number of rows and columns
num_rows <- 100
num_columns <- 45

# Generate random human-like row names
row_names <- randomNames(num_rows)

# Create an empty preference matrix with row names
preference_matrix <- matrix(NA, nrow = num_rows, ncol = num_columns,
                            dimnames = list(row_names, paste0("Term ", 1:num_columns)))

# Fill the preference matrix with unique random numbers
for (i in 1:num_rows) {
  preference_matrix[i, ] <- sample(1:num_columns)
  while (any(duplicated(preference_matrix[i, ]))) {
    preference_matrix[i, ] <- sample(1:num_columns)
  }
}

# Read column names from CSV file starting from A2
column_names <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
column_names <- column_names$X1

# Update column names of preference matrix
colnames(preference_matrix) <- column_names


## Linear Optimization ##

# Define the number of doctors and terms
num_doctors <- nrow(preference_matrix)
num_terms <- ncol(preference_matrix)

# Define the objective function coefficients (preferences)
objective_coeffs <- as.vector(preference_matrix)

#Create a vector of the maximum number of doctors allowed for each term
max_doctors_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term <- as.vector(max_doctors_per_term$X3)


## Linear Optimization with Minimized Dissatisfaction ##

# Calculate dissatisfaction values based on preferences (higher value indicates higher dissatisfaction)
dissatisfaction_values <- num_columns - preference_matrix + 1

# Convert dissatisfaction values to a vector for the objective function
objective_coeffs_dissatisfaction <- as.vector(dissatisfaction_values)

# Create an empty LP model for minimizing dissatisfaction
lp_model_dissatisfaction <- make.lp(0, num_doctors * num_terms)

# Set binary decision variables (0 or 1)
set.type(lp_model_dissatisfaction, columns = 1:(num_doctors * num_terms), type = "binary")

# Set the objective function coefficients for minimizing dissatisfaction
set.objfn(lp_model_dissatisfaction, objective_coeffs_dissatisfaction)

# Add constraints to ensure one assignment per doctor
for (i in 1:num_doctors) {
  constr_one_assignment <- rep(0, num_doctors * num_terms)
  constr_one_assignment[((i - 1) * num_terms + 1):(i * num_terms)] <- 1
  add.constraint(lp_model_dissatisfaction, constr_one_assignment, type = "=", rhs = 1)
}

# Add constraints to limit the maximum number of doctors allocated to each term
for (j in 1:num_terms) {
  constr_max_doctors_per_term <- rep(0, num_doctors * num_terms)
  constr_max_doctors_per_term[j + seq(0, (num_doctors - 1) * num_terms, by = num_terms)] <- 1
  add.constraint(lp_model_dissatisfaction, constr_max_doctors_per_term, type = "<=", rhs = max_doctors_per_term[j])
}


# Print the LP model to check its settings (optional)
print(lp_model_dissatisfaction)

# Solve the linear programming problem to minimize dissatisfaction
lp_result_dissatisfaction <- solve(lp_model_dissatisfaction)

# Check if the LP problem was solved successfully
if (lp_result_dissatisfaction == 0) {
  # Get the optimal solution from the lp_model_dissatisfaction object
  optimal_solution_dissatisfaction <- get.variables(lp_model_dissatisfaction)
 
  # Store doctor-term assignments in a data frame
  optimal_assignments <- data.frame(
    Doctor = character(),
    Term = character(),
    stringsAsFactors = FALSE
  )
  
   
  # Analyze the optimal solution
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (optimal_solution_dissatisfaction[(i - 1) * num_terms + j] == 1) {
        doctor_name <- row_names[i]  # Get the doctor's name using the index
        term_name <- column_names[j]  # Get the term's name using the index
        optimal_assignments <- rbind(optimal_assignments, data.frame(Doctor = doctor_name, Term = term_name))
      }
    }
  }
  
  # Create a gt table to display the optimal solution
  optimal_assignments_table <- gt(optimal_assignments)
  print(optimal_assignments_table)
} else 
  cat("LP problem could not be solved. Result:", lp_result_dissatisfaction, "\n")




