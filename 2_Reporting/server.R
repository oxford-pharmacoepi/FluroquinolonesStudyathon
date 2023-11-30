# server shiny ----
server <- function(input, output, session) {

 # cdm snapshot ----
  output$tbl_cdm_snaphot <- renderText(kable(cdm_snapshot) %>%
                                          kable_styling("striped", full_width = F) )
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(cdm_snapshot)
      gtsave(x, file)
    }
  )
  
 # code use ----
  getCodeUse <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    code_use <- code_use %>% 
      select(c("cdm_name", "codelist_name",
               "group_name", 
               "strata_name", "strata_level",
               "standard_concept_name", "standard_concept_name",
               "source_concept_name",  "source_concept_id" ,   "domain_id",
               "variable_name", "estimate")) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate)
    names(code_use)<-stringr::str_replace_all(names(code_use), "_", " ")
    code_use
      
  })
  
  output$dt_code_use  <- DT::renderDataTable({
    table_data <- getCodeUse()
    
    datatable(table_data, 
              filter = "top",
              rownames= FALSE) 
  })

 # cohort_count ----
  get_cohort_count <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    working_cohort_count <- cohort_count  %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(cohort_name %in%  input$cd_cohort) %>% 
      select(cohort_name, cdm_name, number_records, number_subjects) %>% 
      pivot_wider(names_from = cdm_name, 
                  values_from = c(number_records, number_subjects),
                  names_glue = "{cdm_name}: {.value}",
                  names_vary = "slowest")
    if(isFALSE(input$cd_cc_records)){
      working_cohort_count<-working_cohort_count %>%
      select(!matches("number_record"))
    }
    
    if(isFALSE(input$cd_cc_subjects)) {
      working_cohort_count<-working_cohort_count %>%
        select(!matches("number_subj"))
    }
    
    working_cohort_count
  })
  
  output$dt_cohort_count  <- DT::renderDataTable({
    table_data <- get_cohort_count()
    
    datatable(table_data, rownames= FALSE) 
  })  
 
 # index_codes ----
  get_index_codes <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    index_codes <- index_codes %>% 
    filter(cdm_name %in% input$cd_cdm,
           cohort_name %in%  input$cd_cohort,
           group_name %in%  input$cd_index_group_name,
           strata_name %in%  input$cd_index_strata_name) %>% 
      select(c("cdm_name", "cohort_name" ,
               "group_name", 
               "strata_name", "strata_level",
               "standard_concept_name", "standard_concept_id",
               "source_concept_name",  "source_concept_id" , 
               "variable_name", "estimate")) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate)
    
    
    if(all(input$cd_index_group_name %in%  "Codelist")){
      index_codes <- index_codes %>% 
        select(!c(
          "standard_concept_name", "standard_concept_id",
          "source_concept_name",  "source_concept_id"
        ))
    }
    
    index_codes<-index_codes %>% 
      arrange(desc(`Record count`))
    
    names(index_codes)<-stringr::str_replace_all(names(index_codes), "_", " ")
    
    index_codes

  })
  
  output$dt_index_codes  <- DT::renderDataTable({
    table_data <- get_index_codes()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
 # cohort_intersection ----
  get_cohort_intersection <- reactive({
 
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    cohort_intersection <- cohort_intersection %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(cohort_name_1 %in%  input$cd_cohort) %>%   
      filter(cohort_name_2 %in%  input$cd_cohort) %>%
      select(!c("cohort_definition_id_1", 
               "cohort_definition_id_2"))
    names(cohort_intersection)<-stringr::str_replace_all(names(cohort_intersection), "_", " ")
    
    cohort_intersection
  })
  
  output$dt_cohort_intersection  <- DT::renderDataTable({
    table_data <- get_cohort_intersection()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  
  
 # conceptSummary ----
  get_conceptSummary <- reactive({
    
    validate(
      need(input$ded_cdm != "", "Please select a database")
    )
    validate(
      need(input$ded_cohort != "", "Please select a cohort")
    )
    
    conceptSummary <- conceptSummary %>% 
      filter(cdm_name %in% input$ded_cdm) %>% 
      filter(ingredient %in%  input$ded_cohort) 
    
    conceptSummary
  })
  
  output$dt_conceptSummary  <- DT::renderDataTable({
    table_data <- get_conceptSummary()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  
  
  
 # drugExposureDuration ----
  get_drugExposureDuration <- reactive({
    
    validate(
      need(input$ded_cdm != "", "Please select a database")
    )
    validate(
      need(input$ded_cohort != "", "Please select a cohort")
    )
    
    drugExposureDuration <- drugExposureDuration %>% 
      filter(cdm_name %in% input$ded_cdm) %>% 
      filter(ingredient %in%  input$ded_cohort) 
    
    if(input$ded_dur_byConcept == "Overall per ingredient"){
      drugExposureDuration <- drugExposureDuration %>% 
        filter(type == "Overall") %>% 
        select(!c("drug_concept_id", "drug"))
    } else {
      drugExposureDuration <- drugExposureDuration %>% 
        filter(type != "Overall")
    }
    
    drugExposureDuration <- drugExposureDuration %>% 
      relocate("cdm_name") %>% 
      select(!c("q05_drug_exposure_days",
                "q10_drug_exposure_days",
                "q90_drug_exposure_days",
                "q95_drug_exposure_days",
                "n_non_negative_days",
                "proportion_negative_days",
                "result_obscured" ,
                "type"))
  })
  
  output$dt_drugExposureDuration  <- DT::renderDataTable({
    table_data <- get_drugExposureDuration()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  
  
  
  
 # drugSourceConceptsByConcept ----
  get_drugSourceConceptsByConcept <- reactive({
    
    validate(
      need(input$ded_cdm != "", "Please select a database")
    )
    validate(
      need(input$ded_cohort != "", "Please select a cohort")
    )
    
  drugSourceConceptsByConcept %>% 
      filter(cdm_name %in% input$ded_cdm) %>% 
      filter(ingredient %in%  input$ded_cohort) %>% 
      relocate("cdm_name") %>% 
      filter(result_obscured == FALSE) %>% 
      select(!c( "result_obscured"))
  })
  
  output$dt_drugSourceConceptsByConcept <- DT::renderDataTable({
    table_data <- get_drugSourceConceptsByConcept()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  
  
  
  
  
 # missingValues ----
  get_missingValues <- reactive({
    
    validate(
      need(input$ded_cdm != "", "Please select a database")
    )
    validate(
      need(input$ded_cohort != "", "Please select a cohort")
    )
    
    missingValues <- missingValues %>% 
      filter(cdm_name %in% input$ded_cdm) %>% 
      filter(ingredient %in%  input$ded_cohort) %>% 
      filter(variable %in% input$ded_var)
    
    if(input$ded_miss_byConcept == "Overall per ingredient"){
      missingValues <- missingValues %>% 
        filter(type == "Overall") %>% 
        select(!c("drug_concept_id", "drug"))
    } else {
      missingValues <- missingValues %>% 
        filter(type != "Overall")
    }
    
    missingValues <- missingValues %>% 
      relocate("cdm_name") %>% 
      filter(result_obscured == "FALSE") %>% 
      select(!c("result_obscured" ,
                "n_records_not_missing_value" ,
                "proportion_records_missing_value",
                "type"))
  })
  
  output$dt_missingValues  <- DT::renderDataTable({
    table_data <- get_missingValues()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  
  ## incidence_estimates ----
  ### get estimates ----
  getincidence <- reactive({
    incidence %>%
      filter(cdm_name %in% input$incidence_estimates_cdm_name) %>%
      filter(outcome_cohort_name %in% input$incidence_estimates_outcome_cohort_name) %>%
      filter(denominator_target_cohort_name %in% input$incidence_estimates_denominator_target_cohort_name) %>%
      filter(denominator_age_group %in% input$incidence_estimates_denominator_age_group) %>%
      filter(denominator_sex %in% input$incidence_estimates_denominator_sex) %>%
      filter(denominator_days_prior_observation %in% input$incidence_estimates_denominator_days_prior_observation) %>%
      # filter(denominator_start_date %in% input$incidence_estimates_denominator_start_date) %>%
      # filter(denominator_end_date %in% input$incidence_estimates_denominator_end_date) %>%
      filter(analysis_outcome_washout %in% input$incidence_estimates_analysis_outcome_washout) %>%
      filter(analysis_repeated_events %in% input$incidence_estimates_analysis_repeated_events) %>%
      filter(analysis_complete_database_intervals %in% input$incidence_estimates_analysis_complete_database_intervals) %>%
      # filter(analysis_min_cell_count %in% input$incidence_estimates_analysis_min_cell_count) %>%
      filter(analysis_interval %in% input$incidence_estimates_analysis_interval) %>%
      # filter(incidence_start_date %in% input$incidence_estimates_incidence_start_date) %>%
      mutate(
        person_years = round(suppressWarnings(as.numeric(person_years))),
        person_days = round(suppressWarnings(as.numeric(person_days))),
        n_events = round(suppressWarnings(as.numeric(n_events))),
        incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys))),
        incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower))),
        incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)))
      )
  })
  ### download table ----
  output$incidence_estimates_download_table <- downloadHandler(
    filename = function() {
      "incidenceTable.csv"
    },
    content = function(file) {
      write_csv(getincidence(), file)
    }
  )
  ### table estimates ----
  output$incidence_estimates_table <- renderDataTable({
    table <- getincidence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    table <- table %>%
      mutate(incidence_100000_pys = paste0(
        incidence_100000_pys, " (", incidence_100000_pys_95CI_lower, " to ",
        incidence_100000_pys_95CI_upper, " )"
      )) %>%
      select(cdm_name, outcome_cohort_name, denominator_target_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_100000_pys)
    datatable(
      table,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  ### make plot ----
  plotincidence <- reactive({
    table <- getincidence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))
    plotIncidence(
      table,
      x = input$incidence_estimates_plot_x,
      ylim = c(0, NA),
      facet = input$incidence_estimates_plot_facet,
      colour = input$incidence_estimates_plot_colour,
      colour_name = paste0(input$incidence_estimates_plot_colour, collapse = "; "),
      ribbon = FALSE
    )
  })
  ### download plot ----
  output$incidence_estimates_download_plot <- downloadHandler(
    filename = function() {
      "incidencePlot.png"
    },
    content = function(file) {
      ggsave(
        file,
        plotincidence(),
        width = as.numeric(input$incidence_estimates_download_width),
        height = as.numeric(input$incidence_estimates_download_height),
        dpi = as.numeric(input$incidence_estimates_download_dpi),
        units = "cm"
      )
    }
  )
  ### plot ----
  output$incidence_estimates_plot <- renderPlotly({
    plotincidence()
  })
  
  ## prevalence_estimates ----
  ### get estimates ----
  getprevalence <- reactive({
    prevalence %>%
      filter(cdm_name %in% input$prevalence_estimates_cdm_name) %>%
      filter(outcome_cohort_name %in% input$prevalence_estimates_outcome_cohort_name) %>%
      filter(denominator_target_cohort_name %in% input$prevalence_estimates_denominator_target_cohort_name) %>%
      filter(denominator_age_group %in% input$prevalence_estimates_denominator_age_group) %>%
      filter(denominator_sex %in% input$prevalence_estimates_denominator_sex) %>%
      filter(denominator_days_prior_observation %in% input$prevalence_estimates_denominator_days_prior_observation) %>%
      # filter(denominator_start_date %in% input$prevalence_estimates_denominator_start_date) %>%
      # filter(denominator_end_date %in% input$prevalence_estimates_denominator_end_date) %>%
      filter(analysis_type %in% input$prevalence_estimates_analysis_type) %>%
      # filter(analysis_outcome_lookback_days %in% input$prevalence_estimates_analysis_outcome_lookback_days) %>%
      filter(analysis_time_point %in% input$prevalence_estimates_analysis_time_point) %>%
      filter(analysis_complete_database_intervals %in% input$prevalence_estimates_analysis_complete_database_intervals) %>%
      filter(analysis_full_contribution %in% input$prevalence_estimates_analysis_full_contribution) %>%
      # filter(analysis_min_cell_count %in% input$prevalence_estimates_analysis_min_cell_count) %>%
      filter(analysis_interval %in% input$prevalence_estimates_analysis_interval) %>%
      # filter(prevalence_start_date %in% input$prevalence_estimates_prevalence_start_date) %>%
      mutate(
        n_cases = round(suppressWarnings(as.numeric(n_cases))),
        n_population = round(suppressWarnings(as.numeric(n_population))),
        prevalence = round(suppressWarnings(as.numeric(prevalence)), 4),
        prevalence_95CI_lower = round(suppressWarnings(as.numeric(prevalence_95CI_lower)), 4),
        prevalence_95CI_upper = round(suppressWarnings(as.numeric(prevalence_95CI_upper)), 4)
      )
  })
  ### download table ----
  output$prevalence_estimates_download_table <- downloadHandler(
    filename = function() {
      "prevalenceTable.csv"
    },
    content = function(file) {
      write_csv(getprevalence(), file)
    }
  )
  ### table estimates ----
  output$prevalence_estimates_table <- renderDataTable({
    table <- getprevalence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    table <- table %>%
      mutate(`prevalence (%)` = paste0(
        100 * prevalence, " (", 100 * prevalence_95CI_lower, " to ",
        100 * prevalence_95CI_upper, " )"
      )) %>%
      select(cdm_name, outcome_cohort_name, denominator_target_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_type, analysis_outcome_lookback_days, analysis_time_point, analysis_complete_database_intervals, analysis_full_contribution, analysis_min_cell_count, analysis_interval, prevalence_start_date, n_cases, n_population, "prevalence (%)")
    datatable(
      table,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  ### make plot ----
  plotprevalence <- reactive({
    table <- getprevalence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    class(table) <- c("PrevalenceResult", "IncidencePrevalenceResult", class(table))
    plotPrevalence(
      table,
      x = input$prevalence_estimates_plot_x,
      ylim = c(0, NA),
      facet = input$prevalence_estimates_plot_facet,
      colour = input$prevalence_estimates_plot_colour,
      colour_name = paste0(input$prevalence_estimates_plot_colour, collapse = "; "),
      ribbon = FALSE
    )
  })
  ### download plot ----
  output$prevalence_estimates_download_plot <- downloadHandler(
    filename = function() {
      "prevalencePlot.png"
    },
    content = function(file) {
      ggsave(
        file,
        plotprevalence(),
        width = as.numeric(input$prevalence_estimates_download_width),
        height = as.numeric(input$prevalence_estimates_download_height),
        dpi = as.numeric(input$prevalence_estimates_download_dpi),
        units = "cm"
      )
    }
  )
  ### plot ----
  output$prevalence_estimates_plot <- renderPlotly({
    plotprevalence()
  })
  # patient_characteristics ----
  get_patient_characteristics <- reactive({
    
    validate(
      need(input$chars_cdm != "", "Please select a database")
    )
    validate(
      need(input$chars_cohort != "", "Please select a cohort")
    )
    
    patient_characteristics <- patient_characteristics %>% 
      filter(cdm_name %in% input$chars_cdm) %>% 
      filter(group_level %in%  
               stringr::str_replace_all(
                 stringr::str_to_sentence(input$chars_cohort),
                 "_", " ")
      ) 
    patient_characteristics
  })
  
  output$gt_patient_characteristics  <- render_gt({
    PatientProfiles::gtCharacteristics(get_patient_characteristics())
  })
  output$download_gt_patient_characteristics <- downloadHandler(
    filename = function() {
      "chracteristics.docx"
    },
    content = function(file) {
      PatientProfiles::gtCharacteristics(get_patient_characteristics()) %>%
        gtsave(file)
    }
  )
  
  
  
  
  
  
  
  # lsc ----
  get_large_scale_characteristics <- reactive({
    
    validate(
      need(input$lsc_cdm != "", "Please select a database")
    )
    validate(
      need(input$lsc_cohort != "", "Please select a cohort")
    )
    validate(
      need(input$lsc_time_window != "", "Please select a time window")
    )
    validate(
      need(input$lsc_domain != "", "Please select a domain")
    )
    
    large_scale_characteristics <- large_scale_characteristics %>% 
      filter(cdm_name %in% input$lsc_cdm,
             group_level %in%  input$lsc_cohort,
             variable_level %in%  input$lsc_time_window,
             table_name %in%  input$lsc_domain,
             strata_name %in%  input$lsc_strata_name,
             strata_level %in%  input$lsc_strata_level) %>% 
      select(!c("result_type","group_name",
                "type", "analysis")) %>% 
      pivot_wider(names_from = estimate_type, 
                  values_from = estimate) %>% 
      rename("concept_id" = "concept",
             "concept_name" = "variable",
             "time_window" = "variable_level",
             "domain" = "table_name") %>% 
      relocate("time_window", .after = "domain") %>% 
      filter(cdm_name %in% input$lsc_cdm) %>% 
      filter(group_level %in%  input$lsc_cohort) %>% 
      mutate(percentage = round(percentage, 2)) %>% 
      mutate(count_percentage = paste0(count, " (", percentage, "%)"))
    names(large_scale_characteristics)<-stringr::str_replace_all(names(large_scale_characteristics), "_", " ")
    
    large_scale_characteristics
  })
  
  output$dt_large_scale_characteristics  <- DT::renderDataTable({
    table_data <- get_large_scale_characteristics()
    datatable(table_data, rownames= FALSE) 
  })
  output$download_dt_large_scale_characteristics <- downloadHandler(
    filename = function() {
      "lsc.csv"
    },
    content = function(file) {
      write.csv(get_large_scale_characteristics(), file, row.names = FALSE)
    }
  )
  
  
  
  
  # indication_pediatric ----
  get_indication_pediatric <- reactive({
    
    validate(
      need(input$indication_pediatric_cdm != "", "Please select a database")
    )
    validate(
      need(input$indication_pediatric_cohort != "", "Please select a cohort")
    )
    validate(
      need(input$indication_pediatric_time_window != "", "Please select a time window")
    )
    validate(
      need(input$indication_indication_pediatric != "", "Please select an indication of interest")
    )
    
    indication_pediatric <- indication_pediatric %>% 
      filter(cdm_name %in% input$indication_pediatric_cdm,
             group_level %in%  input$indication_pediatric_cohort,
             variable %in%  input$indication_pediatric_time_window,
             variable_level %in%  input$indication_indication_pediatric,
             strata_name %in%  input$indication_pediatric_strata_name,
             strata_level %in%  input$indication_pediatric_strata_level) 
      
    indication_pediatric %>% 
      pivot_wider(names_from = estimate_type, 
                  values_from = estimate) %>% 
      mutate(percentage = round(as.numeric(percentage), 2)) %>% 
      mutate(count_percentage = paste0(count, " (", percentage, "%)"))
  })
  
  output$dt_indication_pediatric  <- DT::renderDataTable({
    table_data <- get_indication_pediatric()
    datatable(table_data, rownames= FALSE) 
  })
  
  output$download_dt_indication_pediatric <- downloadHandler(
    filename = function() {
      "indication_pediatric.csv"
    },
    content = function(file) {
      write.csv(get_indication_pediatric(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  # indication_adult ----
  get_indication_adult <- reactive({
    
    validate(
      need(input$indication_adult_cdm != "", "Please select a database")
    )
    validate(
      need(input$indication_adult_cohort != "", "Please select a cohort")
    )
    validate(
      need(input$indication_adult_time_window != "", "Please select a time window")
    )
    validate(
      need(input$indication_indication_adult != "", "Please select an indication of interest")
    )
    
    indication_adult <- indication_adult %>% 
      filter(cdm_name %in% input$indication_adult_cdm,
             group_level %in%  input$indication_adult_cohort,
             variable %in%  input$indication_adult_time_window,
             variable_level %in%  input$indication_indication_adult,
             strata_name %in%  input$indication_adult_strata_name,
             strata_level %in%  input$indication_adult_strata_level) 
    
    indication_adult %>% 
      pivot_wider(names_from = estimate_type, 
                  values_from = estimate) %>% 
      mutate(percentage = round(as.numeric(percentage), 2)) %>% 
      mutate(count_percentage = paste0(count, " (", percentage, "%)"))
  })
  
  output$dt_indication_adult  <- DT::renderDataTable({
    table_data <- get_indication_adult()
    datatable(table_data, rownames= FALSE) 
  })   
  
  output$download_dt_indication_adult <- downloadHandler(
    filename = function() {
      "indication_adult.csv"
    },
    content = function(file) {
      write.csv(get_indication_adult(), file, row.names = FALSE)
    }
  )
  
  
  
  
  # dus ----
  get_dus <- reactive({
    
    validate(
      need(input$dus_cdm != "", "Please select a database")
    )
    validate(
      need(input$dus_cohort != "", "Please select a cohort")
    )
    # validate(
    #   need(input$lsc_time_window != "", "Please select a time window")
    # )
    # validate(
    #   need(input$lsc_domain != "", "Please select a domain")
    # )

    dus_summary %>% 
      filter(cdm_name %in% input$dus_cdm,
             group_level %in% input$dus_cohort)
  })
  
  output$dt_dus_duration  <- DT::renderDataTable({
    table_data <- get_dus() %>% 
      filter(variable == "duration")
    datatable(table_data, rownames= FALSE) 
  })
  output$download_dt_dus_duration <- downloadHandler(
    filename = function() {
      "dus_duration.csv"
    },
    content = function(file) {
      write.csv(get_dus() %>% 
                  filter(variable == "duration"), file, row.names = FALSE)
    }
  )
  
  output$dt_dus_initial_dd  <- DT::renderDataTable({
    table_data <- get_dus() %>% 
      filter(variable == "initial_daily_dose_milligram")
    datatable(table_data, rownames= FALSE) 
  })
  output$download_dt_dus_initial_dd <- downloadHandler(
    filename = function() {
      "dus_initial_dd.csv"
    },
    content = function(file) {
      write.csv(get_dus() %>% 
                  filter(variable == "initial_daily_dose_milligram"), file, row.names = FALSE)
    }
  )
  
  output$dt_dus_cumulative_dose  <- DT::renderDataTable({
    table_data <- get_dus() %>% 
      filter(variable == "cumulative_dose_milligram")
    datatable(table_data, rownames= FALSE) 
  })
  output$download_dt_dus_cumulative_dose <- downloadHandler(
    filename = function() {
      "dus_cumulative_dose.csv"
    },
    content = function(file) {
      write.csv(get_dus() %>% 
                  filter(variable == "cumulative_dose_milligram"), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  }

