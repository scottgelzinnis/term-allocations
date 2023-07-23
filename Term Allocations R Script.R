## TERM ALLOCATION PROJECT - FUCK MY LIFE ##

#Packages
library(tidyverse)
library(skimr)
library(readr)

#Specific Rank Data Packages#
library(pmr)

#Read in data
df <- read_csv("Resource Allocations project - Term 1.csv")

#Clean
df$`Term Name` <- factor(df$`Term Name`)
df <- df %>% rename("PGY" = `PGY1 vs PGY2 (3 = Both)`) 
df$PGY <- factor(df$PGY,
                 levels = c(1,2,3),
                 labels = c("PGY1", "PGY2", "PGY1/2"))
df$`Rural vs Urban` <- factor(df$`Rural vs Urban`)
df$`Relief vs Core` <- factor(df$`Relief vs Core`)
df$`Term Classification` <- factor(df$`Term Classification`)
df$`Resource Allocation` <- as.numeric(df$`Resource Allocation`)

#Preferencing Data
  # 125 Unique Terms
    # 56 (44.8%) PGY1 Terms, 49(39.2%) PGY2 Terms, 20(16.0%) Combined Terms
  # 286 Terms/Resources to allocate
  # Required Personnel
    # PGY1 = 128.128 (128)
    # PGY2 = 112.112 (112)
    # Either = 45.76 (46 or 23 of each)
       ## Assume PGY1 = 151 & PGY 2 = 135

# Read in Randomised term preferences # 
