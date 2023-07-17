
# package management ----
renv::restore()

# packages
library(CDMConnector)
library(DrugExposureDiagnostics)
library(PatientProfiles)
library(CodelistGenerator)
library(dplyr)
library(tidyr)
library(gt)
library(here)

# database name
database_name <- "...."

# db connecton
db <- DBI::dbConnect("....")

cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = "....."
)

# generate feasibility report
source(here("RunAnalysis.R"))
