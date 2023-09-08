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
db <- dbConnect(.....)

cdm <- cdmFromCon(con = db,
                  cdmSchema = "....")

source(here("RunAnalysis.R"))
