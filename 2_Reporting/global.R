# load packages -----
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(IncidencePrevalence)

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#78B7C5", #  "#D8DEE9",
    dark_hover_bg = "#3B9AB2", #"#81A1C1",
    dark_color ="white"# "#2E3440"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)
# functions ----
nice.num3<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
nice.num1<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

# read results from data folder ----
results<-list.files(here("data"), recursive = TRUE,
                    full.names = TRUE)

# cdm snapshot ------
cdm_snapshot_files<-results[stringr::str_detect(results, ".csv")]
cdm_snapshot_files<-results[stringr::str_detect(results, "cdm_snapshot")]
cdm_snapshot <- list()
for(i in seq_along(cdm_snapshot_files)){
  cdm_snapshot[[i]]<-readr::read_csv(cdm_snapshot_files[[i]], 
                                     show_col_types = FALSE) %>% 
    select("cdm_name", "person_count", "observation_period_count" ,
           "vocabulary_version")
}
cdm_snapshot <- dplyr::bind_rows(cdm_snapshot)
cdm_snapshot <- cdm_snapshot %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version") %>% 
  distinct()

# cohort_count -----
cohort_count_files<-results[stringr::str_detect(results, ".csv")]
cohort_count_files<-results[stringr::str_detect(results, "cohort_count")]
cohort_count <- list()
for(i in seq_along(cohort_count_files)){
  cohort_count[[i]]<-readr::read_csv(cohort_count_files[[i]], 
                                 show_col_types = FALSE) %>%
    mutate(number_records = if_else(number_records <5 & number_records >0,
                                    as.character("<5"),
                                    as.character(number_records))) %>%
    mutate(number_subjects = if_else(number_subjects <5 & number_subjects >0,
                                     as.character("<5"),
                                     as.character(number_subjects)))
}
cohort_count <- dplyr::bind_rows(cohort_count)


# cohort_intersection -----
cohort_intersection_files<-results[stringr::str_detect(results, ".csv")]
cohort_intersection_files<-results[stringr::str_detect(results, "cohort_intersection")]
cohort_intersection <- list()
for(i in seq_along(cohort_intersection_files)){
  cohort_intersection[[i]]<-readr::read_csv(cohort_intersection_files[[i]], 
                                     show_col_types = FALSE) 
}
cohort_intersection <- dplyr::bind_rows(cohort_intersection)

# index_codes -----
index_codes_files<-results[stringr::str_detect(results, ".csv")]
index_codes_files<-results[stringr::str_detect(results, "index_codes")]
index_codes <- list()
for(i in seq_along(index_codes_files)){
  index_codes[[i]]<-readr::read_csv(index_codes_files[[i]], 
                                    show_col_types = FALSE) 
}
index_codes <- dplyr::bind_rows(index_codes) %>% 
  filter(!is.na(estimate))

# conceptSummary -----
conceptSummary_files<-results[stringr::str_detect(results, ".csv")]
conceptSummary_files<-results[stringr::str_detect(results, "conceptSummary")]
conceptSummary <- list()
for(i in seq_along(conceptSummary_files)){
  conceptSummary[[i]]<-readr::read_csv(conceptSummary_files[[i]], 
                                    show_col_types = FALSE) 
  if(nrow(conceptSummary[[i]])==0){
    conceptSummary[[i]]<- NULL 
  } else {
    conceptSummary[[i]]<-conceptSummary[[i]] %>% 
      mutate(concept_code = as.character(concept_code))
  }
}
conceptSummary <- dplyr::bind_rows(conceptSummary) 


# drugExposureDurationByConcept -----
drugExposureDurationByConcept_files<-results[stringr::str_detect(results, ".csv")]
drugExposureDurationByConcept_files<-results[stringr::str_detect(results, "drugExposureDurationByConcept")]
drugExposureDurationByConcept <- list()
for(i in seq_along(drugExposureDurationByConcept_files)){
  drugExposureDurationByConcept[[i]]<-readr::read_csv(drugExposureDurationByConcept_files[[i]], 
                                       show_col_types = FALSE) 
  if(nrow(drugExposureDurationByConcept[[i]])==0){
    drugExposureDurationByConcept[[i]]<- NULL 
  }
}
drugExposureDurationByConcept <- dplyr::bind_rows(drugExposureDurationByConcept) 



# drugExposureDurationOverall -----
drugExposureDurationOverall_files<-results[stringr::str_detect(results, ".csv")]
drugExposureDurationOverall_files<-results[stringr::str_detect(results, "drugExposureDurationOverall")]
drugExposureDurationOverall <- list()
for(i in seq_along(drugExposureDurationOverall_files)){
  drugExposureDurationOverall[[i]]<-readr::read_csv(drugExposureDurationOverall_files[[i]], 
                                       show_col_types = FALSE) 
  if(nrow(drugExposureDurationOverall[[i]])==0){
    drugExposureDurationOverall[[i]]<- NULL 
  }
}
drugExposureDurationOverall <- dplyr::bind_rows(drugExposureDurationOverall) 


# drugExposureDuration ----
drugExposureDuration <- bind_rows(drugExposureDurationByConcept %>% 
    mutate(type="By concept"),
  drugExposureDurationOverall %>% 
            mutate(type="Overall"))


# drugSourceConceptsByConcept -----
drugSourceConceptsByConcept_files<-results[stringr::str_detect(results, ".csv")]
drugSourceConceptsByConcept_files<-results[stringr::str_detect(results, "drugSourceConceptsByConcept")]
drugSourceConceptsByConcept <- list()
for(i in seq_along(drugSourceConceptsByConcept_files)){
  drugSourceConceptsByConcept[[i]]<-readr::read_csv(drugSourceConceptsByConcept_files[[i]], 
                                       show_col_types = FALSE) 
  if(nrow(drugSourceConceptsByConcept[[i]])==0){
    drugSourceConceptsByConcept[[i]]<- NULL 
  }
}
drugSourceConceptsByConcept <- dplyr::bind_rows(drugSourceConceptsByConcept) 



# missingValuesByConcept -----
missingValuesByConcept_files<-results[stringr::str_detect(results, ".csv")]
missingValuesByConcept_files<-results[stringr::str_detect(results, "missingValuesByConcept")]
missingValuesByConcept <- list()
for(i in seq_along(missingValuesByConcept_files)){
  missingValuesByConcept[[i]]<-readr::read_csv(missingValuesByConcept_files[[i]], 
                                       show_col_types = FALSE) 
  if(nrow(missingValuesByConcept[[i]])==0){
    missingValuesByConcept[[i]]<- NULL 
  }
}
missingValuesByConcept <- dplyr::bind_rows(missingValuesByConcept) 



# missingValuesOverall -----
missingValuesOverall_files<-results[stringr::str_detect(results, ".csv")]
missingValuesOverall_files<-results[stringr::str_detect(results, "missingValuesOverall")]
missingValuesOverall <- list()
for(i in seq_along(missingValuesOverall_files)){
  missingValuesOverall[[i]]<-readr::read_csv(missingValuesOverall_files[[i]], 
                                             show_col_types = FALSE) 
  if(nrow(missingValuesOverall[[i]])==0){
    missingValuesOverall[[i]]<- NULL 
  }
}
missingValuesOverall <- dplyr::bind_rows(missingValuesOverall) 


# missingValues ----
missingValues <- bind_rows(missingValuesByConcept %>% 
                                  mutate(type="By concept"),
                           missingValuesOverall %>% 
                                  mutate(type="Overall")) %>% 
  filter(result_obscured == FALSE)
missingValues <- missingValues %>% 
  mutate(variable = str_replace(variable, "n_missing_" , ""))

# prevalence  ------
prevalence_files<-results[stringr::str_detect(results, ".csv")]
prevalence_files<-prevalence_files[stringr::str_detect(prevalence_files, "prevalence")]
prevalence_files<-prevalence_files[stringr::str_detect(prevalence_files, "attrition", negate = TRUE)]
prevalence <- list()
for(i in seq_along(prevalence_files)){
  prevalence[[i]]<-readr::read_csv(prevalence_files[[i]], 
                                             show_col_types = FALSE) 
}
prevalence <- dplyr::bind_rows(prevalence) %>% 
  mutate(denominator_target_cohort_name = if_else(is.na(denominator_target_cohort_name),
                                                  "General population",
                                                  denominator_target_cohort_name))

# prevalence_attrition  ------
prevalence_attrition_files<-results[stringr::str_detect(results, ".csv")]
prevalence_attrition_files<-prevalence_attrition_files[stringr::str_detect(prevalence_attrition_files, "prevalence")]
prevalence_attrition_files<-prevalence_attrition_files[stringr::str_detect(prevalence_attrition_files, "attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                   show_col_types = FALSE) 
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition) 
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition) %>% 
  mutate(denominator_target_cohort_name = if_else(is.na(denominator_target_cohort_name),
                                                  "General population",
                                                  denominator_target_cohort_name))
# incidence ------
incidence_files<-results[stringr::str_detect(results, ".csv")]
incidence_files<-incidence_files[stringr::str_detect(incidence_files, "incidence")]
incidence_files<-incidence_files[stringr::str_detect(incidence_files, "attrition", negate = TRUE)]
incidence <- list()
for(i in seq_along(incidence_files)){
  incidence[[i]]<-readr::read_csv(incidence_files[[i]], 
                                   show_col_types = FALSE) 
}
incidence <- dplyr::bind_rows(incidence) 
incidence <- dplyr::bind_rows(incidence) %>% 
  mutate(denominator_target_cohort_name = if_else(is.na(denominator_target_cohort_name),
                                                  "General population",
                                                  denominator_target_cohort_name))

# incidence_attrition  ------
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-incidence_attrition_files[stringr::str_detect(incidence_attrition_files, "incidence")]
incidence_attrition_files<-incidence_attrition_files[stringr::str_detect(incidence_attrition_files, "attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                             show_col_types = FALSE) 
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition) 
incidence_attrition <- dplyr::bind_rows(incidence_attrition) %>% 
  mutate(denominator_target_cohort_name = if_else(is.na(denominator_target_cohort_name),
                                                  "General population",
                                                  denominator_target_cohort_name))

# patient_characteristics -----
patient_characteristics_files<-results[stringr::str_detect(results, ".csv")]
patient_characteristics_files<-results[stringr::str_detect(results, "dus_chars")]
patient_characteristics <- list()
for(i in seq_along(patient_characteristics_files)){
  patient_characteristics[[i]]<-readr::read_csv(patient_characteristics_files[[i]], 
                                                show_col_types = FALSE) %>% 
    mutate(estimate = as.character(estimate))
}
patient_characteristics <- dplyr::bind_rows(patient_characteristics)


# large_scale_characteristics -----
large_scale_characteristics_files<-results[stringr::str_detect(results, ".csv")]
large_scale_characteristics_files<-results[stringr::str_detect(results, "dus_lsc")]
large_scale_characteristics <- list()
for(i in seq_along(large_scale_characteristics_files)){
  large_scale_characteristics[[i]]<-readr::read_csv(large_scale_characteristics_files[[i]], 
                                                    show_col_types = FALSE) 
}
large_scale_characteristics <- dplyr::bind_rows(large_scale_characteristics)
table(large_scale_characteristics$strata_name)



# indication_pediatric -----
indication_pediatric_files<-results[stringr::str_detect(results, ".csv")]
indication_pediatric_files<-results[stringr::str_detect(results, "indication_pediatric")]
indication_pediatric <- list()
for(i in seq_along(indication_pediatric_files)){
  indication_pediatric[[i]]<-readr::read_csv(indication_pediatric_files[[i]], 
                                    show_col_types = FALSE) %>% 
    mutate(estimate = as.character(estimate))
}
indication_pediatric <- dplyr::bind_rows(indication_pediatric)

# indication_adult -----
indication_adult_files<-results[stringr::str_detect(results, ".csv")]
indication_adult_files<-results[stringr::str_detect(results, "indication_adult")]
indication_adult <- list()
for(i in seq_along(indication_adult_files)){
  indication_adult[[i]]<-readr::read_csv(indication_adult_files[[i]], 
                                             show_col_types = FALSE) %>% 
    mutate(estimate = as.character(estimate))
}
indication_adult <- dplyr::bind_rows(indication_adult)

# dus -----
dus_summary_files<-results[stringr::str_detect(results, ".csv")]
dus_summary_files<-results[stringr::str_detect(results, "dus_summary")]
dus_summary <- list()
for(i in seq_along(dus_summary_files)){
  print(i)
  dus_summary[[i]]<-readr::read_csv(dus_summary_files[[i]], 
                                                    show_col_types = FALSE)
  if(nrow(dus_summary[[i]]>0)){
    dus_summary[[i]] <- dus_summary[[i]] %>% 
      mutate(estimate = as.character(estimate))   
  }

}
dus_summary <- dplyr::bind_rows(dus_summary)


strataOpsChars <- patient_characteristics %>%
  dplyr::select("strata_name", "strata_level") %>%
  dplyr::distinct() %>%
  dplyr::mutate(strata = paste0(strata_name, ": ", strata_level))
