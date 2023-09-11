#install.packages("renv") # if not already installed, install renv from CRAN
renv::activate()
renv::restore() # this should prompt you to install the various packages required for the study

library(DBI)
library(CDMConnector)
library(DrugExposureDiagnostics)
library(here)

# database name ----
database_name <- "...."

# database connection ----
# see https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
# for examples on how to connect and create your cdm reference
db <- dbConnect(.....)

cdm <- cdmFromCon(.....)

source(here("RunAnalysis.R"))
