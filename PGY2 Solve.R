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


## Linear Optimisation ##

## Linear Optimization ##

# Define the number of doctors and terms
num_doctors <- nrow(preference_matrix)
num_terms <- ncol(preference_matrix)

# Define the objective function coefficients (preferences)
objective_coeffs <- as.vector(preference_matrix)

#Create a vector of the maximum number of doctors allowed for each term
max_doctors_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term <- as.vector(max_doctors_per_term$X3)


# Create an empty LP model
lp_model <- make.lp(0, num_doctors * num_terms)

# Set binary decision variables (0 or 1)
set.type(lp_model, columns = 1:(num_doctors * num_terms), type = "binary")

# Set the objective function coefficients directly as preferences
set.objfn(lp_model, objective_coeffs)

# Add constraints to ensure one assignment per doctor
for (i in 1:num_doctors) {
  constr_one_assignment <- rep(0, num_doctors * num_terms)
  constr_one_assignment[((i - 1) * num_terms + 1):(i * num_terms)] <- 1
  add.constraint(lp_model, constr_one_assignment, type = "=", rhs = 1)
}

# Add constraints to limit the maximum number of doctors allocated to each term
for (j in 1:num_terms) {
  constr_max_doctors_per_term <- rep(0, num_doctors * num_terms)
  constr_max_doctors_per_term[j + seq(0, (num_doctors - 1) * num_terms, by = num_terms)] <- 1
  add.constraint(lp_model, constr_max_doctors_per_term, type = "<=", rhs = max_doctors_per_term[j])
}

# Print the LP model to check its settings (optional)
print(lp_model)

# Solve the linear programming problem
lp_result <- solve(lp_model)

# Check if the LP problem was solved successfully
if (lp_result == 0) {
  # Get the optimal solution from the lp_model object
  optimal_solution <- get.variables(lp_model)
  
  # Analyze the optimal solution
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (optimal_solution[(i - 1) * num_terms + j] == 1) {
        cat("Doctor", i, "is assigned to Term", j, "\n")
      }
    }
  }
} else {
  cat("LP problem could not be solved. Result:", lp_result, "\n")
}







"## REPORTING RESULTS ##
# Extract the allocation results
allocated_vector <- get.variables(lp_model)

# Reshape the allocated vector into a matrix
allocated_matrix <- matrix(allocated_vector, nrow = num_doctors, ncol = num_terms)

# Print the allocated_matrix (1 indicates allocation, 0 indicates no allocation)
rownames(allocated_matrix) <- row_names
colnames(allocated_matrix) <- column_names

# Create a list to store doctor-term assignments
assignments <- list()

# Iterate through the allocated_matrix
for (i in 1:num_doctors) {
  for (j in 1:num_terms) {
    if (allocated_matrix[i, j] == 1) {
      doctor_name <- row_names[i]
      term_name <- column_names[j]
      assignments <- c(assignments, list(c(doctor_name, term_name)))
    }
  }
}

# Convert the list of assignments to a data frame
assignments <- data.frame(doctor = unlist(lapply(assignments, "[[", 1)),
                             term = unlist(lapply(assignments, "[[", 2)))

assignments_table <- gt(assignments)
print(assignments_table)"
                                                                  
                                                                  