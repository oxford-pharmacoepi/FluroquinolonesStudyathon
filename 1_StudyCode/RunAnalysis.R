# start -----
start_time <- Sys.time()
start_temp <- floor(runif(1, min = 1, max = 10)*10)*100
cli::cli_inform("Starting temp tables from {start_temp}")
options(dbplyr_table_name = start_temp)


# cdm reference ----
cli::cli_text("- Creating CDM reference ({Sys.time()})")
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_schema,
                                  write_schema = c(schema = write_schema,
                                                   prefix = study_prefix),
                                  cdm_name = db_name)

# remove problematic concept if it is in vocab
cdm$concept <- cdm$concept %>%
  filter(!concept_id %in% c(43008995, 36852855, 36857452, 36859444, 36861127))

# cdm snapshot ----
cli::cli_text("- Getting cdm snapshot ({Sys.time()})")
write_csv(snapshot(cdm), here("results", paste0(
  "cdm_snapshot_", cdmName(cdm), ".csv"
)))

# get drug concepts ----
cli::cli_text("- Getting drug codes ({Sys.time()})")
drug_ingredients <- cdm$concept %>%
  filter(standard_concept == "S") %>%
  filter(domain_id == "Drug") %>%
  filter(concept_name %in% local(c("ciprofloxacin",
                                   "delafloxacin",
                                   "moxifloxacin",
                                   "ofloxacin",
                                   "levofloxacin",
                                   "norfloxacin"))) %>%
  filter(concept_class_id=="Ingredient")    %>%
  filter(standard_concept =="S") %>%
  select(c("concept_id", "concept_name")) %>%
  collect() %>%
  arrange(concept_name)

# all drug concepts
drug_concepts <- getDrugIngredientCodes(cdm = cdm,
                                                 name = c("ciprofloxacin",
                                                          "delafloxacin",
                                                          "moxifloxacin",
                                                          "ofloxacin",
                                                          "levofloxacin",
                                                          "norfloxacin"))
# systemic forms
drug_concepts_systemic <- getDrugIngredientCodes(cdm = cdm,
                       name = c("ciprofloxacin",
                                "delafloxacin",
                                "moxifloxacin",
                                "ofloxacin",
                                "levofloxacin",
                                "norfloxacin"),
                       doseForm =
                         c("Auto-Injector", "Buccal Film", "Buccal Tablet",
                           "Cartridge", "Chewable Bar", "Chewable Extended Release Oral Tablet",
                           "Chewable Tablet", "Chewing Gum", "Delayed Release Oral Capsule",
                           "Delayed Release Oral Granules", "Delayed Release Oral Tablet", "Disintegrating Oral Tablet",
                           "Drug Implant", "Effervescent Oral Tablet", "Enema",
                           "Extended Release Oral Capsule", "Extended Release Oral Tablet", "Extended Release Suspension",
                           "Granules for Oral Solution", "Granules for Oral Suspension", "Impregnated cigarette",
                           "Injectable Foam", "Injectable Solution", "Injectable Suspension",
                           "Injection", "Intramuscular Solution", "Intraperitoneal Solution",
                           "Intrathecal Suspension", "Intratracheal Suspension", "Intrauterine Drug Delivery System",
                           "Intrauterine System", "Intravenous Solution", "Intravenous Suspension",
                           "Jet Injector", "Mucosal Spray","Nasal Powder",
                           "Nasal Solution", "Nasal Spray","Oral Capsule",
                           "Oral Film", "Oral Flakes","Oral Gel",
                           "Oral Granules", "Oral Lozenge","Oral Pellet",
                           "Oral Powder", "Oral Solution","Oral Spray",
                           "Oral Suspension", "Oral Tablet","Oral Tablet with Sensor",
                           "Oral Wafer", "Pen Injector","Powder for Oral Solution",
                           "Powder for Oral Suspension", "Prefilled Syringe","Rectal Suppository",
                           "Sublingual Film", "Sublingual Powder","Sublingual Tablet",
                           "Suspension", "Sustained Release Buccal Tablet","Tablet for Oral Suspension",
                           "Transdermal System", "Vaginal Powder"))
names(drug_concepts_systemic) <- paste0(names(drug_concepts_systemic) ,
                                        "_systemic")
# add overall groups
drug_concepts[["fluroquinolones"]] <- purrr::list_c(drug_concepts)
drug_concepts_systemic[["fluroquinolones_systemic"]] <- purrr::list_c(drug_concepts_systemic)

# combine
if(isTRUE(ingredient_only)){
  drug_concepts <- purrr::flatten(list(drug_concepts, drug_concepts_systemic))
}

# instantiate concept cohorts -------
cli::cli_text("- Instantiating concept based cohorts - all events included ({Sys.time()})")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm,
                                                         name = "fluro",
                                                         conceptSet = list(fluroquinolones = drug_concepts[["fluroquinolones"]]),
                                                         gapEra = 7,
                                                         limit = "all")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm,
                                                         name = "study_cohorts",
                                                         conceptSet = drug_concepts,
                                                         gapEra = 7,
                                                         limit = "all")
# drug utilisation cohorts -----
cli::cli_text("- Creating DUS cohorts ({Sys.time()})")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm,
                                                         name = "study_cohorts_dus",
                                                         conceptSet = drug_concepts,
                                                         gapEra = 7,
                                                         limit = "all",
                                                         priorUseWashout = 30,
                                                         priorObservation = 30,
                                                         cohortDateRange =  as.Date(c("2012-01-01",
                                                                                      "2022-01-01")))


# subset cdm -----
cli::cli_text("- Subsetting cdm to DUS cohorts ({Sys.time()})")
cdm_dus <- cdm_subset_cohort(
  cdm = cdm,
  cohort_table = "fluro"
)
cdm_dus$person <- cdm_dus$person %>% computeQuery()
cdm_dus$observation_period <- cdm_dus$observation_period %>% computeQuery()
cdm_dus$condition_occurrence <- cdm_dus$condition_occurrence %>% computeQuery()
cdm_dus$drug_exposure <- cdm_dus$drug_exposure %>% computeQuery()

# indication cohorts --------
cli::cli_text("- Creating indication cohorts ({Sys.time()})")

indications_def <- read_csv(here("indications",
              "Fluoroquinolones_potIndications_v3.csv"),
         show_col_types = FALSE)
indications_def$decision_children<- CodelistGenerator:::tidyWords(indications_def$decision_children)
indications_def$decision_adult<- CodelistGenerator:::tidyWords(indications_def$decision_adult)

indications_pediatrics<-indications_def %>%
  filter(!is.na(decision_children)) %>%
  select(c("concept_id", "decision_children"))
indications_pediatrics<- split(indications_pediatrics,
      f = indications_pediatrics$decision_children)
for(i in seq_along(indications_pediatrics)){
  indications_pediatrics[[i]]<-indications_pediatrics[[i]] %>% pull("concept_id")
}

indications_adult<-indications_def %>%
  filter(!is.na(decision_adult)) %>%
  select(c("concept_id", "decision_adult"))
indications_adult<- split(indications_adult,
                               f = indications_adult$decision_adult)
for(i in seq_along(indications_adult)){
  indications_adult[[i]]<-indications_adult[[i]] %>% pull("concept_id")
}

cdm_dus <- CDMConnector::generate_concept_cohort_set(cdm = cdm_dus,
                                                     concept_set = indications_pediatrics,
                                                     name = "indications_pediatrics",
                                                     end = 1,
                                                     limit = "all",
                                                     overwrite = TRUE)
cdm_dus <- CDMConnector::generate_concept_cohort_set(cdm = cdm_dus,
                                                     concept_set = indications_adult,
                                                     name = "indications_adult",
                                                     end = 1,
                                                     limit = "all",
                                                     overwrite = TRUE)


# run drug exposure diagnostics ----
cli::cli_text("- Running drug exposure diagnostics ({Sys.time()})")
drug_diagnostics <- suppressWarnings(suppressMessages(
  # uninformative warnings because we're not running all checks
  DrugExposureDiagnostics::executeChecks(
    cdm = cdm_dus,
    ingredients = drug_ingredients$concept_id,
    subsetToConceptId = unique(purrr::list_c(drug_concepts)),
    checks = c(
      "missing",
      "exposureDuration",
      "sourceConcept"
    ))
))

for(i in seq_along(drug_diagnostics)){
  write_csv(drug_diagnostics[[i]] %>%
              mutate(cdm_name = !!db_name),
            here("results", paste0(
              names(drug_diagnostics)[i], "_" ,cdmName(cdm), ".csv"
            )))

}

# cohort counts ----
cli::cli_text("- Instantiating cohorts ({Sys.time()})")
cohort_counts <- cohort_count(cdm[["study_cohorts"]]) %>%
  left_join(cohort_set(cdm[["study_cohorts"]]),
            by = "cohort_definition_id") %>%
  mutate(cdm_name = db_name)
cohort_counts <- cohort_counts %>%
  mutate(number_records = if_else(number_records <5 & number_records >0,
                                  as.character("<5"),
                                  as.character(number_records))) %>%
  mutate(number_subjects = if_else(number_subjects <5 & number_subjects >0,
                                   as.character("<5"),
                                   as.character(number_subjects)))

write_csv(cohort_counts,
          here("results", paste0(
            "cohort_count_", cdmName(cdm), ".csv"
          )))

# index events  ----
cli::cli_text("- Getting index event codes ({Sys.time()})")
index_codes<- list()
non_empty_cohorts <- sort(cohort_count(cdm[["study_cohorts"]]) %>%
                            filter(number_records > 0) %>%
                            pull("cohort_definition_id"))

for(i in seq_along(non_empty_cohorts)){
  working_cohort_id <- non_empty_cohorts[i]
  working_cohort <- cohort_set(cdm[["study_cohorts"]]) %>%
    filter(cohort_definition_id == working_cohort_id) %>%
    pull("cohort_name")
  cli::cli_text("-- For {working_cohort} ({i} of {length(non_empty_cohorts)})")

  index_codes[[i]] <- summariseCohortCodeUse(drug_concepts[working_cohort] ,
                                             cohortTable = "study_cohorts",
                                             cohortId = working_cohort_id,
                                             timing = "entry",
                                             cdm = cdm,
                                             byYear = TRUE,
                                             bySex = TRUE,
                                             ageGroup = list(c(0,17),
                                                             c(18,150))) %>%
    mutate(cohort_name = working_cohort)

}
index_codes <- bind_rows(index_codes) %>%
  mutate(cdm_name = db_name)
write_csv(index_codes,
          here("results", paste0(
            "index_codes_", cdmName(cdm), ".csv"
          )))


# incidence and prevalence: general population -----
if(isTRUE(run_incidence_prevalence)){
cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator",
                                    ageGroup = list(c(0, 150),
                                                    c(0, 17),
                                                    c(18, 59), c(60, 150),
                                                    # pediatric
                                                    c(0, 1), c(1, 4), c(5, 9),
                                                    c(10, 14), c(15, 17)
                                                    ),
                                    cohortDateRange = as.Date(c("2012-01-01", "2022-01-01")),
                                    sex = c("Both", "Male", "Female"),
                                    daysPriorObservation = c(30),
                                    overwrite = TRUE)
inc_gpop <- estimateIncidence(cdm, denominatorTable = "denominator",
                              outcomeTable = "study_cohorts",
                              interval = c("quarters", "years"),
                              completeDatabaseIntervals = TRUE,
                              outcomeWashout = 30,
                              repeatedEvents = TRUE)
write_csv(inc_gpop,
          here("results", paste0(
            "incidence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(incidenceAttrition(inc_gpop),
          here("results", paste0(
            "incidence_attrition_general_population_", cdmName(cdm), ".csv"
          )))


cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator_for_months",
                                    ageGroup = list(c(19, 59), c(60, 150)),
                                    cohortDateRange = as.Date(c("2012-01-01", "2022-01-01")),
                                    sex = c("Both"),
                                    daysPriorObservation = c(30),
                                    overwrite = TRUE)
inc_gpop_months <- estimateIncidence(cdm,
                                     denominatorTable = "denominator_for_months",
                              outcomeTable = "study_cohorts",
                              interval = c("months"),
                              completeDatabaseIntervals = TRUE,
                              outcomeWashout = 30,
                              repeatedEvents = TRUE)
write_csv(inc_gpop_months,
          here("results", paste0(
            "incidence_general_population_months_", cdmName(cdm), ".csv"
          )))
write_csv(incidenceAttrition(inc_gpop_months),
          here("results", paste0(
            "incidence_attrition_general_population_months_", cdmName(cdm), ".csv"
          )))

prev_gpop <- estimatePeriodPrevalence(cdm,
                                      denominatorTable = "denominator",
                                      outcomeTable = "study_cohorts",
                                      interval = c("quarters", "years"),
                                      completeDatabaseIntervals = TRUE,
                                      fullContribution = TRUE)
write_csv(prev_gpop,
          here("results", paste0(
            "prevalence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(prevalenceAttrition(prev_gpop),
          here("results", paste0(
            "prevalence_attrition_general_population_", cdmName(cdm), ".csv"
          )))
}

# incidence and prevalence: hospitalised -----
if(isTRUE(run_incidence_prevalence)){
hospitalisation_codes <- getDescendants(cdm, c(9201, 262)) %>%
  select("concept_id")

DBI::dbWriteTable(conn = attr(cdm, "dbcon"),
                  name = inSchema(attr(cdm, "write_schema"), "hospitalisation_codes"),
                  value = hospitalisation_codes)

cdm$hospitalisation_codes <- tbl(attr(cdm, "dbcon"),
                                 inSchema(attr(cdm, "write_schema"), "hospitalisation_codes"))

cdm$hospitalisations <- cdm$hospitalisation_codes %>%
  inner_join(cdm$visit_occurrence %>%
              select("person_id",
                     "visit_start_date", "visit_end_date",
                     "visit_concept_id"),
            by = c("concept_id"="visit_concept_id")) %>%
  select(!c("concept_id")) %>%
  rename("subject_id" = "person_id",
         "cohort_start_date" = "visit_start_date",
         "cohort_end_date" = "visit_end_date") %>%
  mutate(cohort_definition_id = 1L) %>%
  relocate("cohort_definition_id") %>%
  distinct() %>%
  computeQuery(name = "hosp", temporary = FALSE,
               schema = attr(cdm, "write_schema"),
               overwrite = TRUE)

DBI::dbRemoveTable(conn = attr(cdm, "dbcon"),
                   name = inSchema(attr(cdm, "write_schema"), "hospitalisation_codes"))


if(cdm$hospitalisations %>% head(5) %>% tally() %>%  pull() > 0){

cdm$hospitalisations <- cdm$hospitalisations %>%
  new_generated_cohort_set(overwrite = TRUE,
                           cohort_set_ref =  data.frame(cohort_definition_id = 1L,
                                                        cohort_name = "hospitalised"))

cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator_hosp",
                                    ageGroup = list(c(0, 150),
                                                    c(0, 17),
                                                    c(18, 59),
                                                    c(60, 150),
                                                    # pediatric
                                                    c(0, 1), c(1, 4), c(5, 9),
                                                    c(10, 14), c(15, 17)
                                    ),
                                    targetCohortTable = "hospitalisations",
                                    cohortDateRange = as.Date(c("2012-01-01", "2022-01-01")),
                                    sex = c("Both", "Male", "Female"),
                                    daysPriorObservation = c(30),
                                    overwrite = TRUE)

inc_hosp <- estimateIncidence(cdm, denominatorTable = "denominator_hosp",
                              outcomeTable = "study_cohorts",
                              interval = c("quarters", "years"),
                              completeDatabaseIntervals = TRUE,
                              outcomeWashout = 30,
                              repeatedEvents = TRUE)
write_csv(inc_hosp,
          here("results", paste0(
            "incidence_hospitalised_", cdmName(cdm), ".csv"
          )))
write_csv(incidenceAttrition(inc_hosp),
          here("results", paste0(
            "incidence_attrition_hospitalised_", cdmName(cdm), ".csv"
          )))

prev_hosp <- estimatePeriodPrevalence(cdm,
                                      denominatorTable = "denominator_hosp",
                                      outcomeTable = "study_cohorts",
                                      interval = c("quarters", "years"),
                                      completeDatabaseIntervals = TRUE,
                                      fullContribution = TRUE)
write_csv(prev_hosp,
          here("results", paste0(
            "prevalence_hospitalised_", cdmName(cdm), ".csv"
          )))
write_csv(prevalenceAttrition(prev_hosp),
          here("results", paste0(
            "prevalence_attrition_hospitalised_", cdmName(cdm), ".csv"
          )))

}

}

# patient characteristics -----
cli::cli_text("- Getting demographics of DUS cohorts ({Sys.time()})")
cdm_dus$study_cohorts_dus <- cdm_dus$study_cohorts_dus %>%
  addAge(ageGroup =
           list(c(0, 17),
                 c(18, 59),
                 c(60, 150)))

cdm_dus$study_cohorts_dus <- cdm_dus$study_cohorts_dus %>%
  mutate(time_period = if_else(cohort_start_date < as.Date("2019-04-01"),
                               "Pre", "Post"))


dus_chars <- cdm_dus$study_cohorts_dus %>%
  PatientProfiles::summariseCharacteristics(
    ageGroup = list(c(0, 17), c(18, 59), c(60, 150)),
    strata = list(c("age_group"), c("time_period"), c("time_period", "age_group")),
    conceptSetIntersect = list(
      "Tests" = list(
        conceptSet = codesFromConceptSet(path = here("tests_14daysbefore_7daysafter"), cdm = cdm),
        window = list(c(-14, 7)),
        value = "flag"
      ),
      "Comorbidities any time prior" = list(
        conceptSet = codesFromConceptSet(path = here("comorbidities_anytime"), cdm = cdm),
        window = list(c(-Inf, 0)),
        value = "flag"
      ),
      "Medications 30 days prior" = list(
        conceptSet = codesFromConceptSet(path = here("comedication_30days"), cdm = cdm),
        window = list(c(-30, 0)),
        value = "flag"
      ),
      "Medications 1 year prior" = list(
        conceptSet = codesFromConceptSet(path = here("comedication_1yr"), cdm = cdm),
        window = list(c(-365, 0)),
        value = "flag"
      )
    )
  )

dus_chars <- dus_chars %>%
  filter(!variable %in% c("Cohort start date",
                          "Cohort end date")) %>%
  filter(!(variable == "Age" &
        estimate_type %in% c("min", "q05", "q95", "max"))) %>%
  filter(!estimate_type %in% c("q05", "q95"))


write_csv(dus_chars,
          here("results", paste0(
            "dus_chars_summary_", cdmName(cdm), ".csv"
          )))

# large scale characterisation -----
cli::cli_text("- Running large scale characterisation of DUS cohorts ({Sys.time()})")
dus_lsc <- PatientProfiles::summariseLargeScaleCharacteristics(cdm_dus$study_cohorts_dus,
                                                                    eventInWindow = c("condition_occurrence"),
                                                               window = list(c(-30, -1), c(0, 0)),
                                                               strata = list(c("age_group"), c("time_period"), c("time_period", "age_group")))
write_csv(dus_lsc,
          here("results", paste0(
            "dus_lsc_summary_", cdmName(cdm), ".csv"
          )))

# indications: pediatric -----
cli::cli_text("- Summarising pediatric indications for DUS cohorts ({Sys.time()})")
dus_pediatric_indication <- cdm_dus$study_cohorts_dus %>%
 filter(age <= 17) %>%
  addIndication(
    indicationCohortName = "indications_pediatrics",
    indicationGap =  c(0, 7, 30),
    unknownIndicationTable = "condition_occurrence"
  ) %>%
  summariseIndication(strata = list(c("age_group"), c("time_period"), c("time_period", "age_group")))

write_csv(dus_pediatric_indication,
          here("results", paste0(
            "dus_indication_pediatric_", cdmName(cdm), ".csv"
          )))

# indications: adult -----
cli::cli_text("- Summarising adult indications for DUS cohorts ({Sys.time()})")
dus_adult_indication <- cdm_dus$study_cohorts_dus %>%
   filter(age > 17) %>%
  addIndication(
    indicationCohortName = "indications_adult",
    indicationGap =  c(0, 7, 30),
    unknownIndicationTable = "condition_occurrence"
  ) %>%
  summariseIndication(strata = list(c("age_group"), c("time_period"), c("time_period", "age_group")))

write_csv(dus_adult_indication,
          here("results", paste0(
            "dus_indication_adult_", cdmName(cdm), ".csv"
          )))

# DUS details -----
if(isFALSE(ingredient_only)){
# go ingredient by ingredient
cli::cli_text("- Summarising drug utilisation ({Sys.time()})")
non_empty_cohorts <- sort(cohort_count(cdm_dus[["study_cohorts_dus"]]) %>%
                            filter(number_records > 0) %>%
                            left_join(cohort_set(cdm_dus[["study_cohorts_dus"]]),
                                      by = "cohort_definition_id")%>%
                            filter(cohort_name != "fluroquinolones") %>%
                            pull("cohort_definition_id"))

dus_summary_cohorts <- cohort_set(cdm_dus[["study_cohorts_dus"]]) %>%
  filter(str_detect(cohort_name, "systemic")) %>%
  filter(str_detect(cohort_name, "fluroquinolone", negate = TRUE)) %>%
  filter(cohort_definition_id %in% non_empty_cohorts) %>%
  pull(cohort_definition_id)

dus_summary <- list()
cli::cli_text("- Summarising duration and dose of DUS cohorts ({Sys.time()})")
for(i in seq_along(dus_summary_cohorts)){
working_cohort_name <- cohort_set(cdm_dus[["study_cohorts_dus"]]) %>%
  filter(cohort_definition_id == dus_summary_cohorts[i]) %>%
  pull(cohort_name)

working_ingredient <- drug_ingredients %>%
  filter(concept_name == str_replace(working_cohort_name, "_systemic","")) %>%
  pull(concept_id)

cli::cli_text("-- Summarising duration and dose of systemic {names(drug_concepts)[i]} ({i} of {length(dus_summary_cohorts)}) ({Sys.time()})")
dus_summary[[i]] <- cdm_dus$study_cohorts_dus %>%
  addDrugUse(
    ingredientConceptId = working_ingredient,
    conceptSet = drug_concepts[working_cohort_name],
    duration = TRUE,
    quantity = FALSE,
    dose = TRUE,
    gapEra = 7,
    eraJoinMode = "zero",
    overlapMode = "sum",
    sameIndexMode = "sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ) %>%
  summariseDrugUse(strata = list(c("age_group"), c("time_period"), c("time_period", "age_group")))

dus_summary[[i]] <- dus_summary[[i]] %>%
  filter(group_level == working_cohort_name)

}
dus_summary <- bind_rows(dus_summary)
write_csv(dus_summary,
          here("results", paste0(
            "dus_summary_", cdmName(cdm), ".csv"
          )))
}

# zip all results -----
cli::cli_text("- Zipping results ({Sys.time()})")
files_to_zip <- list.files(here("results"))
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        db_name)]
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        ".csv")]

zip::zip(zipfile = file.path(paste0(
  here("results"), "/results_", db_name, ".zip"
)),
files = files_to_zip,
root = here("results"))

dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
cli::cli_alert_success("Study code finished")
cli::cli_alert_success(glue::glue(
  "Code ran in {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
))
