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


## Linear Optimization - Defining the problem ##

# Define the number of doctors and terms
num_doctors <- nrow(preference_matrix)
num_terms <- ncol(preference_matrix)

# Define the objective function coefficients (preferences)
objective_coeffs <- as.vector(preference_matrix)

#Create a vector of the maximum number of doctors allowed for each term
max_doctors_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term <- as.vector(max_doctors_per_term$X3)

#Create a vector that describes "Specialty" status of a term
specialty_status_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
specialty_status_per_term <- as.vector(specialty_status_per_term$X8)
specialty_status_per_term <- as.factor(specialty_status_per_term)

#Create a vector that describes "Sub-Specialty" status of a term
sub_specialty_status_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
sub_specialty_status_per_term <- as.vector(sub_specialty_status_per_term$X9)
sub_specialty_status_per_term <- as.factor(sub_specialty_status_per_term)

#Create a vector that describes "Term Classification A" of a term
term_classification_A <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
term_classification_A <- as.vector(term_classification_A$X6)
term_classification_A <- as.factor(term_classification_A)

#Create a vector that describes "Term Classification B" of a term
term_classification_B <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
term_classification_B <- as.vector(term_classification_B$X7)
term_classification_B <- as.factor(term_classification_B)

## Linear Optimization with Minimized Dissatisfaction ##

# Calculate objective coefficients based on top preferences
top_preference_coefficients <- matrix(0, nrow = num_rows, ncol = num_columns)
for (i in 1:num_rows) {
  top_preference_coefficients[i, ] <- ifelse(preference_matrix[i, ] %in% c(1, 2), 2, 1)
}
objective_coeffs_top_preference <- as.vector(top_preference_coefficients)

# Create an empty LP model for maximizing top preferences
lp_model_top_preference <- make.lp(0, num_doctors * num_terms)

# Set binary decision variables (0 or 1)
set.type(lp_model_top_preference, columns = 1:(num_doctors * num_terms), type = "binary")

# Modify the LP model to maximize top preferences
objective_coeffs_top_preference <- as.vector(preference_matrix)
set.objfn(lp_model_top_preference, objective_coeffs_top_preference)  # No negative sign

# Add constraints to ensure one assignment per doctor (same as before)
for (i in 1:num_doctors) {
  constr_one_assignment <- rep(0, num_doctors * num_terms)
  constr_one_assignment[((i - 1) * num_terms + 1):(i * num_terms)] <- 1
  add.constraint(lp_model_top_preference, constr_one_assignment, type = "=", rhs = 1)
}

# Add constraints to limit the maximum number of doctors allocated to each term (same as before)
for (j in 1:num_terms) {
  constr_max_doctors_per_term <- rep(0, num_doctors * num_terms)
  constr_max_doctors_per_term[j + seq(0, (num_doctors - 1) * num_terms, by = num_terms)] <- 1
  add.constraint(lp_model_top_preference, constr_max_doctors_per_term, type = "<=", rhs = max_doctors_per_term[j])
}

# Print the LP model to check its settings (optional)
print(lp_model_top_preference)

# Solve the linear programming problem to maximize top preferences
lp_result_top_preference <- solve(lp_model_top_preference)

# Check if the LP problem was solved successfully
if (lp_result_top_preference == 0) {
  # Get the optimal solution from the lp_model_top_preference object
  optimal_solution_top_preference <- get.variables(lp_model_top_preference)
  
  # Store doctor-term assignments in a data frame
  optimal_assignments_top_preference <- data.frame(
    Doctor = character(),
    Term = character(),
    stringsAsFactors = FALSE
  )
  
  # Analyze the optimal solution
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (optimal_solution_top_preference[(i - 1) * num_terms + j] == 1) {
        doctor_name <- row_names[i]  # Get the doctor's name using the index
        term_name <- column_names[j]  # Get the term's name using the index
        optimal_assignments_top_preference <- rbind(optimal_assignments_top_preference, data.frame(Doctor = doctor_name, Term = term_name))
      }
    }
  }
  
  # Create a gt table to display the optimal solution
  optimal_assignments_table_top_preference <- gt(optimal_assignments_top_preference)
  print(optimal_assignments_table_top_preference)
} else 
  cat("LP problem could not be solved. Result:", lp_result_top_preference, "\n")









## NEXT STEP TO RUN A LOOP TO ASSIGN 5 TERMS ACROSS THE YEAR BASED ON PREFERENCES ##

# Number of times to run the assignment
num_assignments <- 5

# Initialize a list to store the results of each assignment
assignment_results_top_preference <- list()

# Loop through the assignments
for (assignment_num in 1:num_assignments) {
  # Create an empty LP model for maximizing top preferences
  lp_model_top_preference <- make.lp(0, num_doctors * num_terms)
  
  # Set binary decision variables (0 or 1)
  set.type(lp_model_top_preference, columns = 1:(num_doctors * num_terms), type = "binary")
  
  # Set the objective function coefficients for maximizing top preferences
  set.objfn(lp_model_top_preference, objective_coeffs_top_preference)
  
  # Add constraints to ensure one assignment per doctor
  for (i in 1:num_doctors) {
    constr_one_assignment <- rep(0, num_doctors * num_terms)
    constr_one_assignment[((i - 1) * num_terms + 1):(i * num_terms)] <- 1
    add.constraint(lp_model_top_preference, constr_one_assignment, type = "=", rhs = 1)
  }
  
  # Add constraints to limit the maximum number of doctors allocated to each term
  for (j in 1:num_terms) {
    constr_max_doctors_per_term <- rep(0, num_doctors * num_terms)
    constr_max_doctors_per_term[j + seq(0, (num_doctors - 1) * num_terms, by = num_terms)] <- 1
    add.constraint(lp_model_top_preference, constr_max_doctors_per_term, type = "<=", rhs = max_doctors_per_term[j])
  }
  
  # Solve the linear programming problem to maximize top preferences
  lp_result_top_preference <- solve(lp_model_top_preference)
  
  # Check if the LP problem was solved successfully
  if (lp_result_top_preference == 0) {
    optimal_solution_top_preference <- get.variables(lp_model_top_preference)
    assignment_results_top_preference[[assignment_num]] <- optimal_solution_top_preference
  } else {
    cat("LP problem could not be solved for assignment", assignment_num, "Result:", lp_result_top_preference, "\n")
  }
}

# Print the results of each assignment
for (assignment_num in 1:num_assignments) {
  cat("Assignment", assignment_num, "results:\n")
  optimal_solution_top_preference <- assignment_results_top_preference[[assignment_num]]
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (optimal_solution_top_preference[(i - 1) * num_terms + j] == 1) {
        doctor_name <- row_names[i]
        term_name <- column_names[j]
        cat("Doctor", doctor_name, "is assigned to Term", term_name, "\n")
      }
    }
  }
  cat("\n")
}

# Create a data frame to store the assignment results for each doctor
assignment_results_df_top_preference <- data.frame(
  Doctor = row_names, 
  stringsAsFactors = FALSE
)

# Initialize a matrix to track assigned terms for each doctor
assigned_terms_matrix_top_preference <- matrix(FALSE, nrow = num_doctors, ncol = num_terms)

# Iterate through the assignments
for (assignment_num in 1:num_assignments) {
  assignment_column <- paste0("Assignment ", assignment_num)
  assignment_results_df_top_preference[[assignment_column]] <- NA
  
  for (i in 1:num_doctors) {
    # Filter unassigned terms for this doctor
    unassigned_terms <- column_names[!assigned_terms_matrix_top_preference[i, ]]
    
    if (length(unassigned_terms) > 0) {
      # Calculate a combined score that considers top preferences (lower values)
      combined_score <- ifelse(preference_matrix[i, ] %in% c(1, 2), -2, -1)  # Note the negative values
      
      # Sort terms based on the combined score
      sorted_terms <- unassigned_terms[order(combined_score[match(unassigned_terms, column_names)])]
      
      # Assign the doctor to the term with the highest combined score
      assigned_term <- sorted_terms[1]
      assigned_term_index <- match(assigned_term, column_names)
      
      # Mark the term as assigned for this doctor
      assigned_terms_matrix_top_preference[i, assigned_term_index] <- TRUE
      
      # Update the data frame with the assigned term
      assignment_results_df_top_preference[[assignment_column]][i] <- assigned_term
    }
  }
}

# Print the table using gt
assignment_results_table_top_preference <- gt(assignment_results_df_top_preference)
print(assignment_results_table_top_preference)


## Further reporting to add transparency to allocations ##

# Initialize an empty data frame to store the report
doctor_preference_report <- data.frame(
  Doctor = character(),
  Term = character(),
  Preference_Score = numeric(),
  stringsAsFactors = FALSE
)

# Iterate through each doctor
for (i in 1:num_doctors) {
  doctor_name <- row_names[i]
  
  # Iterate through each of the 5 terms allocated to the doctor
  for (assignment_num in 1:num_assignments) {
    assigned_term <- assignment_results_df_top_preference[[paste0("Assignment ", assignment_num)]][i]
    
    # Access the doctor's preference row from the preference matrix
    doctor_preference <- preference_matrix[i, ]
    
    # Check if 'assigned_term' is not missing
    if (!is.na(assigned_term)) {
      # Calculate preference score for the assigned term
      preference_score <- doctor_preference[match(assigned_term, column_names)]
      
      # Build a data frame with the results for this doctor and term
      doctor_report <- data.frame(
        Doctor = rep(doctor_name, 1),
        Term = assigned_term,
        Preference_Score = preference_score
      )
      
      # Append the doctor's report to the overall report data frame
      doctor_preference_report <- rbind(doctor_preference_report, doctor_report)
    }
  }
}

# Print the report using gt
doctor_preference_report_table <- gt(doctor_preference_report)
print(doctor_preference_report_table)
