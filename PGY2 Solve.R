#Set WD
setwd("D:/My Documents/GitHub/term-allocations")

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

df <- read_csv("PGY2 Solve.csv")

# Define the number of doctors and terms
num_doctors <- nrow(preference_matrix)
num_terms <- ncol(preference_matrix)

# Define the objective function coefficients (preferences)
objective_coeffs <- as.vector(preference_matrix)

#Create a vector of the maximum number of doctors allowed for Term 1
max_doctors_per_term1 <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term1 <- as.vector(max_doctors_per_term1$X3)

#Create a vector of the maximum number of doctors allowed for Term 2
max_doctors_per_term2 <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term2 <- as.vector(max_doctors_per_term2$X4)

#Create a vector of the maximum number of doctors allowed for Term 3
max_doctors_per_term3 <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term3 <- as.vector(max_doctors_per_term3$X5)

#Create a vector of the maximum number of doctors allowed for Term 4
max_doctors_per_term4 <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term4 <- as.vector(max_doctors_per_term4$X6)

#Create a vector of the maximum number of doctors allowed for Term 5
max_doctors_per_term5 <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
max_doctors_per_term5 <- as.vector(max_doctors_per_term5$X7)

#Create a vector that describes the Clinical Team structure of a term
clinical_structure_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
clinical_structure_term <- as.vector(clinical_structure_term$X13)
clinical_structure_term <- as.factor(clinical_structure_term)

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

# Map longer names to A, B, C, D, E
term_classification_mapping <- list(
  "A-Undifferentiated illness patient care"      = "A",
  "B-Chronic illness patient care"               = "B",
  "C-Acute critical illness patient care"        = "C",
  "D-Peri-operative / procedural patient care"   = "D",
  "E-Non-direct clinical experience (PGY2 Only)" = "E"
)

# Modify the term_classification_A vector
term_classification_A <- as.vector(unlist(term_classification_A))
term_classification_A <- term_classification_mapping[term_classification_A]

#Create a vector that describes "Term Classification B" of a term
term_classification_B <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
term_classification_B <- as.vector(term_classification_B$X7)
term_classification_B <- as.factor(term_classification_B)
term_classification_B <- term_classification_mapping[term_classification_B]


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

# 1. Add constraints to ensure one assignment per doctor
for (i in 1:num_doctors) {
  constr_one_assignment <- rep(0, num_doctors * num_terms)
  constr_one_assignment[((i - 1) * num_terms + 1):(i * num_terms)] <- 1
  add.constraint(lp_model_dissatisfaction, constr_one_assignment, type = "=", rhs = 1)
}

# 2. Add constraints to limit the maximum number of doctors allocated to each term
for (j in 1:num_terms) {
  constr_max_doctors_per_term <- rep(0, num_doctors * num_terms)
  constr_max_doctors_per_term[j + seq(0, (num_doctors - 1) * num_terms, by = num_terms)] <- 1
  add.constraint(lp_model_dissatisfaction, constr_max_doctors_per_term, type = "<=", rhs = max_doctors_per_term[j])
}

# 3. Constraints for Specialty Status:
unique_specialty_statuses <- unique(specialty_status_per_term[!is.na(specialty_status_per_term)])

for (specialty in unique_specialty_statuses) {
  for (i in 1:num_doctors) {
    constr_specialty <- rep(0, num_doctors * num_terms)
    for (j in which(specialty_status_per_term == specialty)) {
      constr_specialty[(i - 1) * num_terms + j] <- 1
    }
    add.constraint(lp_model_dissatisfaction, constr_specialty, type = "<=", rhs = 2)
  }
}

# 4. Constraints for Sub-Specialty Status:
unique_sub_specialties <- unique(sub_specialty_status_per_term[!is.na(sub_specialty_status_per_term)])

for (sub_specialty in unique_sub_specialties) {
  for (i in 1:num_doctors) {
    constr_sub_specialty <- rep(0, num_doctors * num_terms)
    for (j in which(sub_specialty_status_per_term == sub_specialty)) {
      constr_sub_specialty[(i - 1) * num_terms + j] <- 1
    }
    add.constraint(lp_model_dissatisfaction, constr_sub_specialty, type = "<=", rhs = 1)
  }
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








## NEXT STEP TO RUN A LOOP TO ASSIGN 5 TERMS ACROSS THE YEAR BASED ON PREFERENCES ##

# Number of times to run the assignment
num_assignments <- 5

# Initialize a list to store the results of each assignment
assignment_results <- list()

# This matrix will have rows corresponding to doctors and columns corresponding to terms.
# An entry of 1 in the matrix indicates that the doctor (row) has been assigned the term (column).
assigned_terms_matrix <- matrix(0, nrow = num_doctors, ncol = num_terms)

# Loop through the assignments
for (assignment_num in 1:num_assignments) {
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
  
  # Add constraints to prevent doctors from being assigned to the same term again
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (assigned_terms_matrix[i, j] == 1) {
        constraint_already_assigned <- rep(0, num_doctors * num_terms)
        constraint_already_assigned[(i - 1) * num_terms + j] <- 1
        add.constraint(lp_model_dissatisfaction, constraint_already_assigned, type = "=", rhs = 0)
      }
    }
  }
  
  # 3. Constraints for Specialty Status:
  unique_specialty_statuses <- unique(specialty_status_per_term[!is.na(specialty_status_per_term)])
  
  for (specialty in unique_specialty_statuses) {
    for (i in 1:num_doctors) {
      constr_specialty <- rep(0, num_doctors * num_terms)
      for (j in which(specialty_status_per_term == specialty)) {
        constr_specialty[(i - 1) * num_terms + j] <- 1
      }
      add.constraint(lp_model_dissatisfaction, constr_specialty, type = "<=", rhs = 2)
    }
  }
  
  # 4. Constraints for Sub-Specialty Status:
  unique_sub_specialties <- unique(sub_specialty_status_per_term[!is.na(sub_specialty_status_per_term)])
  
  for (sub_specialty in unique_sub_specialties) {
    for (i in 1:num_doctors) {
      constr_sub_specialty <- rep(0, num_doctors * num_terms)
      for (j in which(sub_specialty_status_per_term == sub_specialty)) {
        constr_sub_specialty[(i - 1) * num_terms + j] <- 1
      }
      add.constraint(lp_model_dissatisfaction, constr_sub_specialty, type = "<=", rhs = 1)
    }
  }
  
  # Solve the linear programming problem to minimize dissatisfaction
  lp_result_dissatisfaction <- solve(lp_model_dissatisfaction)
  
  # Check if the LP problem was solved successfully
  if (lp_result_dissatisfaction == 0) {
    optimal_solution_dissatisfaction <- get.variables(lp_model_dissatisfaction)
    assignment_results[[assignment_num]] <- optimal_solution_dissatisfaction
    
    # Update the assigned_terms_matrix based on this solution
    for (i in 1:num_doctors) {
      for (j in 1:num_terms) {
        if (optimal_solution_dissatisfaction[(i - 1) * num_terms + j] == 1) {
          assigned_terms_matrix[i, j] <- 1
        }
      }
    }
  } else {
    cat("LP problem could not be solved for assignment", assignment_num, "Result:", lp_result_dissatisfaction, "\n")
  }
}

# Print the results of each assignment
for (assignment_num in 1:num_assignments) {
  cat("Assignment", assignment_num, "results:\n")
  optimal_solution_dissatisfaction <- assignment_results[[assignment_num]]
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (optimal_solution_dissatisfaction[(i - 1) * num_terms + j] == 1) {
        doctor_name <- row_names[i]
        term_name <- column_names[j]
        cat("Doctor", doctor_name, "is assigned to Term", term_name, "\n")
      }
    }
  }
  cat("\n")
}
# Initialize a data frame to store the results
assignment_table <- data.frame(Doctor = row_names, 
                               Assignment1 = character(num_doctors), 
                               Assignment2 = character(num_doctors),
                               Assignment3 = character(num_doctors),
                               Assignment4 = character(num_doctors),
                               Assignment5 = character(num_doctors),
                               stringsAsFactors = FALSE)

# Populate the data frame with the assignment results
for (assignment_num in 1:num_assignments) {
  optimal_solution_dissatisfaction <- assignment_results[[assignment_num]]
  for (i in 1:num_doctors) {
    for (j in 1:num_terms) {
      if (optimal_solution_dissatisfaction[(i - 1) * num_terms + j] == 1) {
        term_name <- column_names[j]
        assignment_column <- paste0("Assignment", assignment_num)
        assignment_table[i, assignment_column] <- term_name
      }
    }
  }
}

# Print the table using gt
gt_table <- gt(assignment_table)
print(gt_table)


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
  
  # Iterate through each of the assignments to extract the allocated terms for each doctor
  for (assignment_num in 1:num_assignments) {
    optimal_solution_dissatisfaction <- assignment_results[[assignment_num]]
    for (j in 1:num_terms) {
      if (optimal_solution_dissatisfaction[(i - 1) * num_terms + j] == 1) {
        assigned_term <- column_names[j]
        
        # Access the doctor's preference row from the preference matrix
        doctor_preference <- preference_matrix[i, ]
        
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
}

# Print the report using gt
doctor_preference_report_table <- gt(doctor_preference_report)
print(doctor_preference_report_table)

