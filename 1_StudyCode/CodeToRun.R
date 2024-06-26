#install.packages("renv") # if not already installed, install renv from CRAN
renv::activate()
renv::restore(repos = c(CRAN = "https://cloud.r-project.org")) # this should prompt you to install the various packages required for the study

# Load packages ------
library(DBI)
library(CDMConnector)
library(DrugExposureDiagnostics)
library(dplyr)
library(here)
library(dplyr)
library(RPostgres)
library(odbc)
library(CodelistGenerator)
library(PatientProfiles)
library(CohortCharacteristics)
library(DrugUtilisation)
library(IncidencePrevalence)
library(readr)
library(tidyr)
library(zip)
library(stringr)
library(testthat)
library(remotes)

packageVersion("PatientProfiles") # should be 0.8
packageVersion("CodelistGenerator") # should be 2.2.3
packageVersion("DrugUtilisation") # should be 0.5.3
packageVersion("IncidencePrevalence") # should be 0.7.2


# database name ----
db_name <- "...."

# database connection ----
db <- dbConnect(.....)
cdm_schema <- "...."
write_schema <- "...."

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "fluro_"

# run analysis ----
run_drug_diagnostics <- TRUE
run_incidence_prevalence <- TRUE
source(here("RunAnalysis.R"))
