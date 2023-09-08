
# load drug concepts ----
# note, these were identified in drug_concepts/concept_search.R
drug_concepts <- read.csv(here("drug_concepts", "drug_concepts.csv"))

# run diagnostics ----
drug_diagnostics <- suppressWarnings(suppressMessages(
  # uninformative warnings because we're not running all checks
  DrugExposureDiagnostics::executeChecks(
  cdm = cdm,
  ingredients = drug_concepts$concept_id,
  checks = c(
    "missing",
    "exposureDuration",
    "sourceConcept"
  ))
))

# export diagnostics ----
writeResultToDisk(drug_diagnostics,
                  databaseId = database_name,
                  outputFolder = here("results"))
