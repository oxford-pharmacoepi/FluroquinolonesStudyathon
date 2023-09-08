#install.packages("renv") # if not already installed, install renv from CRAN
renv::activate()
renv::restore() # this should prompt you to install the various packages required for the study

library(DBI)
library(CDMConnector)
library(DrugExposureDiagnostics)
library(dplyr)
library(here)

# database name ----
database_name <- "CPRD GOLD"

# database connection ----
server_dbi<-"cdm_gold_202207"
user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)
cdm <- cdmFromCon(con = db,
                  cdmSchema = "public",
                  writeSchema = "results",
                  cdmName = database_name)


source(here("RunAnalysis.R"))
