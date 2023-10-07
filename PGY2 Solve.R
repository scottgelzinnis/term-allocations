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
names(clinical_structure_term) <- df[[1]]


#Create a vector that describes "Specialty" status of a term
specialty_status_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
specialty_status_per_term <- as.vector(specialty_status_per_term$X8)
specialty_status_per_term <- as.factor(specialty_status_per_term)

#Create a vector that describes "Sub-Specialty" status of a term
sub_specialty_status_per_term <- read_csv("PGY2 Solve.csv", col_names = FALSE, skip = 1)
sub_specialty_status_per_term <- as.vector(sub_specialty_status_per_term$X9)
sub_specialty_status_per_term <- as.factor(sub_specialty_status_per_term)

#Create a vector that describes "Term Classification 1" of a term
term_classification1 <- df$`Term Classification`
term_classification1 <- as_factor(term_classification1)


#Create a vector that describes "Term Classification 2" of a term
term_classification2 <- df$`Term Classification 2`
term_classification2 <- as_factor(term_classification2)


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
  
# 1. Add constraints to limit the maximum number of doctors allocated to each term
  
  # Helper function to retrieve the correct 'max doctors' vector for a given term
  get_max_doctors_for_term <- function(term) {
    switch(term,
           "1" = max_doctors_per_term1,
           "2" = max_doctors_per_term2,
           "3" = max_doctors_per_term3,
           "4" = max_doctors_per_term4,
           "5" = max_doctors_per_term5)
  }
  
  # Add constraints based on the unique requirements of each term
  for (j in 1:num_terms) {
    constr_max_doctors_per_term <- rep(0, num_doctors * num_terms)
    
    # Indices for this term's assignment for all doctors
    indices <- j + seq(0, (num_doctors - 1) * num_terms, by = num_terms)
    constr_max_doctors_per_term[indices] <- 1
    
    # Get the respective 'max doctors' vector for this term
    max_doctors_for_this_term <- get_max_doctors_for_term(assignment_num)
    
    add.constraint(lp_model_dissatisfaction, constr_max_doctors_per_term, type = "<=", rhs = max_doctors_for_this_term[j])
  }
  
  
# 2. Add constraints to prevent doctors from being assigned to the same term again
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

# # 5. Term Classification Constraint - Must have a term in Cat A, Cat B and Cat C. 
# 
#   # Step 1: Pre-process the classifications
#   classification_A <- matrix(0, nrow = num_doctors, ncol = num_terms)
#   classification_B <- matrix(0, nrow = num_doctors, ncol = num_terms)
#   classification_C <- matrix(0, nrow = num_doctors, ncol = num_terms)
#   
#   for (j in 1:num_terms) {
#     if ("A-Undifferentiated illness patient care" %in% c(term_classification1[j], term_classification2[j])) {
#       classification_A[, j] <- 1
#     }
#     if ("B-Chronic illness patient care" %in% c(term_classification1[j], term_classification2[j])) {
#       classification_B[, j] <- 1
#     }
#     if ("C-Acute critical illness patient care" %in% c(term_classification1[j], term_classification2[j])) {
#       classification_C[, j] <- 1
#     }
#   }
#   
#   # Calculate the number of A, B, and C assignments each doctor already has from previous iterations
#   previous_A_assignments <- rowSums(assigned_terms_matrix * classification_A)
#   previous_B_assignments <- rowSums(assigned_terms_matrix * classification_B)
#   previous_C_assignments <- rowSums(assigned_terms_matrix * classification_C)
#   
#   # Set high penalty values for not having A, B, C terms
#   penalty_A <- 1000 # Adjust as needed
#   penalty_B <- 1000
#   penalty_C <- 1000
#   
#   # Adjust the objective function coefficients based on penalties
#   for (i in 1:num_doctors) {
#     indices_A <- which(classification_A[i, ] == 1)
#     indices_B <- which(classification_B[i, ] == 1)
#     indices_C <- which(classification_C[i, ] == 1)
#     
#     if (previous_A_assignments[i] == 0) {
#       objective_coeffs_dissatisfaction[seq(i, num_doctors*num_terms, by=num_doctors)[indices_A]] <- 
#         objective_coeffs_dissatisfaction[seq(i, num_doctors*num_terms, by=num_doctors)[indices_A]] - penalty_A
#     }
#     if (previous_B_assignments[i] == 0) {
#       objective_coeffs_dissatisfaction[seq(i, num_doctors*num_terms, by=num_doctors)[indices_B]] <- 
#         objective_coeffs_dissatisfaction[seq(i, num_doctors*num_terms, by=num_doctors)[indices_B]] - penalty_B
#     }
#     if (previous_C_assignments[i] == 0) {
#       objective_coeffs_dissatisfaction[seq(i, num_doctors*num_terms, by=num_doctors)[indices_C]] <- 
#         objective_coeffs_dissatisfaction[seq(i, num_doctors*num_terms, by=num_doctors)[indices_C]] - penalty_C
#     }
#   }
#   
#   # Update the objective function of the LP model with the adjusted coefficients
#   set.objfn(lp_model_dissatisfaction, objective_coeffs_dissatisfaction)
  

  
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

# Sort by the Doctor's first name
assignment_table_sorted <- assignment_table %>%
  arrange(str_extract(Doctor, "^[\\w-]+"))

# Print the table using gt
gt_table <- gt(assignment_table_sorted)
print(gt_table)


                         
## Further reporting to add transparency to allocations ##

# 1. Doctors Preference Report

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

# Sort by the Doctor's first name
doctor_preference_report_sorted <- doctor_preference_report %>%
  arrange(str_extract(Doctor, "^[\\w-]+"))

# Print the report using gt
doctor_preference_report_table <- gt(doctor_preference_report_sorted)
print(doctor_preference_report_table)



# 2. HETI Compliance Report


# Calculate average preference for each doctor across all 5 assignments
avg_preference <- doctor_preference_report %>%
  group_by(Doctor) %>%
  summarize(Avg_Score = mean(Preference_Score, na.rm = TRUE))

# Check if a doctor has been assigned to more than 1 term in any sub-specialty
sub_specialty_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  assigned_terms_df <- subset(doctor_preference_report, Doctor == doctor_name)
  terms_assigned <- assigned_terms_df$Term
  sub_specialty_terms <- sub_specialty_status_per_term[terms_assigned]
  max_count <- max(table(sub_specialty_terms), na.rm = TRUE)
  return(max_count <= 1)
})

# Check if a doctor has been assigned to more than 2 terms in any specialty
specialty_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  terms_assigned <- doctor_preference_report$Term[doctor_preference_report$Doctor == doctor_name]
  specialty_terms <- specialty_status_per_term[terms_assigned]
  max_count <- max(table(specialty_terms), na.rm = TRUE)
  return(max_count <= 2)
})

# Check if a doctor has been assigned to at least 3 "Team Based" terms
team_based_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  terms_assigned <- doctor_preference_report$Term[doctor_preference_report$Doctor == doctor_name]
  
  # Debugging print statements
  print(paste("Doctor:", doctor_name))
  print(paste("Assigned terms:", toString(terms_assigned)))
  structure_indices <- match(terms_assigned, names(clinical_structure_term))
  print(paste("Matching indices:", toString(structure_indices)))
  print(paste("Clinical structures:", toString(clinical_structure_term[structure_indices])))
  
  team_based_count <- sum(clinical_structure_term[structure_indices] == "Team Based")
  
  return(team_based_count >= 3)
})

# Check for compliance with the desired term classifications
check_term_classifications <- function(terms_assigned) {
  classification1 <- term_classification1[match(terms_assigned, column_names)]
  classification2 <- term_classification2[match(terms_assigned, column_names)]
  required_terms <- c("A-Undifferentiated illness patient care", 
                      "B-Chronic illness patient care", 
                      "C-Acute critical illness patient care")
  
  # Check for each term individually, without merging the classifications
  has_required_terms <- sapply(required_terms, function(term) {
    term %in% classification1 || term %in% classification2
  })
  
  return(all(has_required_terms))
}

term_classifications_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  terms_assigned <- doctor_preference_report$Term[doctor_preference_report$Doctor == doctor_name]
  return(check_term_classifications(terms_assigned))
})

compliance_data <- data.frame(
  Doctor = avg_preference$Doctor,
  Avg_Preference = avg_preference$Avg_Score,
  SubSpecialty_Compliance = sub_specialty_compliance,
  Specialty_Compliance = specialty_compliance,
  Team_Based_Compliance = team_based_compliance,
  Term_Classifications_Compliance = term_classifications_compliance
)

# Format with gt

HETI_compliance_table <- compliance_data %>%
  gt() %>%
  cols_label(
    Avg_Preference = "Avg. Preference (1-5)",
    SubSpecialty_Compliance = "Max 1 Term in Sub-Specialty",
    Specialty_Compliance = "Max 2 Terms in Specialty",
    Team_Based_Compliance = "At Least 3 'Team Based' Terms",
    Term_Classifications_Compliance = "Essential Classifications"
  )

print(HETI_compliance_table)


### MESSING AROUND WITH POST PROCESSING TO ACHIEVE PERFECT HETI COMPLIANCE ###

# Define the swap_terms function to encapsulate the swap logic
swap_terms <- function(matrix, doctor1, doctor2, term1, term2) {
  matrix[doctor1, term1] <- 0
  matrix[doctor1, term2] <- 1
  matrix[doctor2, term1] <- 1
  matrix[doctor2, term2] <- 0
  return(matrix)
}

# Define a function to get the benefit of a swap between two doctors for a particular term
swap_benefit <- function(assigned_terms_matrix, doctor1, doctor2, term1, term2, preferences) {
  original_benefit_d1 = preferences[doctor1, term1] + preferences[doctor2, term2]
  swapped_benefit_d1 = preferences[doctor1, term2] + preferences[doctor2, term1]
  return(swapped_benefit_d1 - original_benefit_d1)
}

# Utility functions to check constraints
is_valid_swap <- function(matrix, doctor1, doctor2, term1, term2, clinical_structure_term, term_classification1, term_classification2) {
  matrix <- swap_terms(matrix, doctor1, doctor2, term1, term2)
  
  # Ensure the Team Based term requirement is met
  if(sum(matrix[doctor1, clinical_structure_term == "Team Based"]) < 3) {
    cat("Doctor", doctor1, "has less than 3 Team Based terms after swap.\n")
    return(FALSE)
  }
  
  if(sum(matrix[doctor2, clinical_structure_term == "Team Based"]) < 3) {
    cat("Doctor", doctor2, "has less than 3 Team Based terms after swap.\n")
    return(FALSE)
  }
  
  # Get the terms for each doctor after the swap
  terms_doctor1 <- which(matrix[doctor1,] == 1)
  terms_doctor2 <- which(matrix[doctor2,] == 1)
  
  combined_classifications_doctor1 <- c(term_classification1[terms_doctor1], term_classification2[terms_doctor1])
  combined_classifications_doctor2 <- c(term_classification1[terms_doctor2], term_classification2[terms_doctor2])
  
  # Check if doctor1 has all required categories across both classification vectors
  if(sum(c("Cat A", "Cat B", "Cat C") %in% combined_classifications_doctor1) < 3) {
    cat("Doctor", doctor1, "does not have all required categories across both classifications after swap.\n")
    return(FALSE)
  }
  
  # Check if doctor2 has all required categories across both classification vectors
  if(sum(c("Cat A", "Cat B", "Cat C") %in% combined_classifications_doctor2) < 3) {
    cat("Doctor", doctor2, "does not have all required categories across both classifications after swap.\n")
    return(FALSE)
  }
  
  return(TRUE)
}

post_process_with_constraints <- function(assigned_terms_matrix, preference_matrix, term_classification1, term_classification2, clinical_structure_term) {
  num_doctors <- nrow(assigned_terms_matrix)
  num_terms <- ncol(assigned_terms_matrix)
  
  # Initialize an empty list to store the swaps
  swaps_report <- list()
  
  for(i in 1:(num_doctors-1)) {
    for(j in (i+1):num_doctors) {
      for(term1 in 1:num_terms) {
        for(term2 in 1:num_terms) {
          if(assigned_terms_matrix[i, term1] == 1 && assigned_terms_matrix[j, term2] == 1) {
            
            # Print out the current evaluation for debugging
            cat("Currently evaluating Doctor1:", rownames(preference_matrix)[i], "Doctor2:", rownames(preference_matrix)[j], "Term1:", colnames(preference_matrix)[term1], "Term2:", colnames(preference_matrix)[term2], "\n")
            
            benefit <- swap_benefit(assigned_terms_matrix, i, j, term1, term2, preference_matrix)
            if(benefit > 0) {
              valid_swap <- is_valid_swap(assigned_terms_matrix, i, j, term1, term2, clinical_structure_term, term_classification1, term_classification2)
              if(valid_swap) {
                assigned_terms_matrix <- swap_terms(assigned_terms_matrix, i, j, term1, term2)
                
                # Store the swap details in the list
                swaps_report[[length(swaps_report) + 1]] <- list(
                  Doctor1 = rownames(preference_matrix)[i],
                  Doctor2 = rownames(preference_matrix)[j],
                  Term1_swapped = colnames(preference_matrix)[term1],
                  Term2_swapped = colnames(preference_matrix)[term2],
                  Benefit = benefit
                )
              }
            }
          }
        }
      }
    }
  }
  
  return(list(AssignedMatrix = assigned_terms_matrix, Report = swaps_report))
}

# Function to display the report
display_swaps_report <- function(swaps_report) {
  cat("Swap Report:\n\n")
  for(swap in swaps_report) {
    cat("Doctor:", swap$Doctor1, "swapped", swap$Term1_swapped, "with\n")
    cat("Doctor:", swap$Doctor2, "who had", swap$Term2_swapped, "\n")
    cat("Benefit of this swap:", swap$Benefit, "\n\n")
  }
}

# Use the function
results <- post_process_with_constraints(assigned_terms_matrix, preference_matrix, term_classification1, term_classification2, clinical_structure_term)
display_swaps_report(results$Report)

## Adjusted Assignments ##
adjusted_assignments <- post_process_with_constraints(assigned_terms_matrix, preferences, term_classification1, term_classification2, clinical_structure_term)


# 1. Doctors Preference Report (Adjusted)

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
    optimal_solution_dissatisfaction <- adjusted_assignments # Using the adjusted assignments
    for (j in 1:num_terms) {
      if (optimal_solution_dissatisfaction[i, j] == 1) { # Note the change in index
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


# 2. HETI Compliance Report (Adjusted)


# Calculate average preference for each doctor across all 5 assignments
avg_preference <- aggregate(Preference_Score ~ Doctor, data = doctor_preference_report, mean)

# Check if a doctor has been assigned to more than 1 term in any sub-specialty
sub_specialty_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  assigned_terms_df <- subset(doctor_preference_report, Doctor == doctor_name)
  terms_assigned <- assigned_terms_df$Term
  sub_specialty_terms <- sub_specialty_status_per_term[terms_assigned]
  max_count <- max(table(sub_specialty_terms), na.rm = TRUE)
  return(max_count <= 1)
})

# Check if a doctor has been assigned to more than 2 terms in any specialty
specialty_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  terms_assigned <- doctor_preference_report$Term[doctor_preference_report$Doctor == doctor_name]
  specialty_terms <- specialty_status_per_term[terms_assigned]
  max_count <- max(table(specialty_terms), na.rm = TRUE)
  return(max_count <= 2)
})

# Check if a doctor has been assigned to at least 3 "Team Based" terms
team_based_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  terms_assigned <- doctor_preference_report$Term[doctor_preference_report$Doctor == doctor_name]
  
  # Debugging print statements
  print(paste("Doctor:", doctor_name))
  print(paste("Assigned terms:", toString(terms_assigned)))
  structure_indices <- match(terms_assigned, names(clinical_structure_term))
  print(paste("Matching indices:", toString(structure_indices)))
  print(paste("Clinical structures:", toString(clinical_structure_term[structure_indices])))
  
  team_based_count <- sum(clinical_structure_term[structure_indices] == "Team Based")
  
  return(team_based_count >= 3)
})

# Check for compliance with the desired term classifications
check_term_classifications <- function(terms_assigned) {
  classification1 <- term_classification1[match(terms_assigned, column_names)]
  classification2 <- term_classification2[match(terms_assigned, column_names)]
  required_terms <- c("A-Undifferentiated illness patient care", 
                      "B-Chronic illness patient care", 
                      "C-Acute critical illness patient care")
  
  # Check for each term individually, without merging the classifications
  has_required_terms <- sapply(required_terms, function(term) {
    term %in% classification1 || term %in% classification2
  })
  
  return(all(has_required_terms))
}

term_classifications_compliance <- sapply(1:num_doctors, function(i) {
  doctor_name <- row_names[i]
  terms_assigned <- doctor_preference_report$Term[doctor_preference_report$Doctor == doctor_name]
  return(check_term_classifications(terms_assigned))
})

compliance_data <- data.frame(
  Doctor = row_names,
  Avg_Preference = avg_preference$Preference_Score,
  SubSpecialty_Compliance = sub_specialty_compliance,
  Specialty_Compliance = specialty_compliance,
  Team_Based_Compliance = team_based_compliance,
  Term_Classifications_Compliance = term_classifications_compliance
)

# Format with gt

HETI_compliance_table <- compliance_data %>%
  gt() %>%
  cols_label(
    Avg_Preference = "Avg. Preference (1-5)",
    SubSpecialty_Compliance = "Max 1 Term in Sub-Specialty",
    Specialty_Compliance = "Max 2 Terms in Specialty",
    Team_Based_Compliance = "At Least 3 'Team Based' Terms",
    Term_Classifications_Compliance = "Essential Classifications"
  )

print(HETI_compliance_table)

