#install.packages("renv") # if not already installed, install renv from CRAN
# renv::activate()
renv::restore(repos = c(CRAN = "https://cloud.r-project.org")) # this should prompt you to install the various packages required for the study

# Load packages ------
library(DBI)
library(duckdb)
library(omopgenerics) # 0.2.3
library(CDMConnector) #1.4.0
library(DrugExposureDiagnostics) # 1.0.6
library(dplyr)
library(here)
library(dplyr)
library(RPostgres)
library(odbc)
library(CodelistGenerator) # 3.0
library(PatientProfiles) # 1.1.0
library(CohortCharacteristics) # 0.2.1
library(DrugUtilisation) # 0.6.1
library(IncidencePrevalence) #0.7.4
library(readr)
library(tidyr)
library(zip)
library(stringr)
library(testthat)
library(remotes)

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

# create cdm reference -----
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_schema,
                                  write_schema = c(schema = write_schema,
                                                   prefix = study_prefix),
                                  cdm_name = db_name)

# you should see the first few rows of your person table if you uncomment the next line
# cdm$person


# run analysis ----
run_drug_diagnostics <- FALSE
run_incidence_prevalence <- FALSE
use_duckdb <- FALSE # will collect to duckdb for incprev - faster, but requires more memory
source(here("RunAnalysis.R"))
