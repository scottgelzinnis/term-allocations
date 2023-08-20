#Set WD
setwd("C:/Users/Scotty/Desktop/Allocations Linear Optimisation")

# Install and load the necessary package
install.packages("randomNames")
install.packages("lpSolve")
install.packages("lpSolveAPI")
install.packages("ROI")



# Load necessary libraries
library(readr)
library(tidyverse)
library(skimr)
library(lpSolve)
library(randomNames)
library(lpSolveAPI)
library(ROI)


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

# Define the number of doctors and terms
num_doctors <- nrow(preference_matrix)
num_terms <- ncol(preference_matrix)

# Define the objective function coefficients (preferences)
objective_coeffs <- as.vector(preference_matrix)

# Define the maximum number of doctors that can be allocated to each term
max_doctors_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term <- as.vector(max_doctors_per_term$X3)

# Define the constraint matrix for doctor allocations to terms
# Each row represents a doctor's allocation to terms
A <- matrix(0, nrow = num_doctors, ncol = num_terms * num_doctors)
for (i in 1:num_doctors) {
  A[i, ((i - 1) * num_terms + 1):(i * num_terms)] <- 1
}

# Define the right-hand side vector for the doctor allocation constraints
b <- rep(1, num_doctors)

# Define the constraint matrix for term allocations to doctors
# Each row represents a term's allocation to doctors
A_eq <- matrix(0, nrow = num_terms, ncol = num_doctors * num_terms)
for (i in 1:num_terms) {
  A_eq[i, ((i - 1) * num_doctors + 1):(i * num_doctors)] <- 1
}

# Define the right-hand side vector for the term allocation constraints
b_eq <- rep(1, num_terms)

# Solve the linear programming problem
allocation <- lp(direction = "min",
                 objective.in = objective_coeffs,
                 const.mat = rbind(A, A_eq),
                 const.dir = rep(c("<=", "=="), c(num_doctors, num_terms)),
                 const.rhs = c(b, b_eq))

# Extract the allocation results
allocated_vector <- allocation$solution
allocated_matrix <- matrix(allocated_vector, nrow = num_doctors, ncol = num_terms)

# Print the allocated_matrix (1 indicates allocation, 0 indicates no allocation)
rownames(allocated_matrix) <- rownames(preference_matrix)
colnames(allocated_matrix) <- colnames(preference_matrix)

# Create an empty list to store doctor-term assignments
assignments <- list()

# Iterate through the allocated_matrix
for (i in 1:num_doctors) {
  for (j in 1:num_terms) {
    if (allocated_matrix[i, j] == 1) {
      doctor_name <- rownames(allocated_matrix)[i]
      term_name <- colnames(allocated_matrix)[j]
      assignments <- c(assignments, list(c(doctor_name, term_name)))
    }
  }
}

# Print the list of doctor-term assignments
print(assignments)


