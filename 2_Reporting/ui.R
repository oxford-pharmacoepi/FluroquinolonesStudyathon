# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Databases",
        tabName = "dbs",
        menuSubItem(
          text = "Database details",
          tabName = "cdm_snapshot"
        )
      ),
      menuItem(
        text = "Study diagnostics",
        tabName = "cd",
        menuSubItem(
          text = "Drug exposure diagnostics",
          tabName = "drug_exposure_diagnostics"
        ),
        menuSubItem(
          text = "Cohort diagnostics",
          tabName = "cohort_diagnostics"
        )
      ),
      menuItem(
        text = "Study results",
        tabName = "study_results",
        menuSubItem(
          text = "Population-level incidence",
          tabName = "incidence"
        ),
        menuSubItem(
          text = "Population-level prevalence",
          tabName = "prevalence"
        ),
        menuSubItem(
          text = "Patient demographics",
          tabName = "chars"
        ),
        menuSubItem(
          text = "Indication",
          tabName = "indication"
        ),
        menuSubItem(
          text = "Large scale characteriscs",
          tabName = "lsc"
        ),
        menuSubItem(
          text = "Drug utilisation",
          tabName = "dus"
        )
      )
)
),

  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
  # background  ------
      tabItem(
        tabName = "background",
        h3("Use of systemic fluoroquinolones in primary care and hospital settings in the UK: a drug utilisation study"),
        tags$hr(),
        a(img(src="logo.png", align = "right",
              height="2%", width="20%"), href="https://www.ohdsi-europe.org/index.php/national-nodes/uk",
          target="_blank")
      ),
  # cdm snapshot ------
      tabItem(
        tabName = "cdm_snapshot",
        htmlOutput('tbl_cdm_snaphot'),
        tags$hr(),
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_cdm_snaphot_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
      ),
  # drug exposure diagnostics ------
  tabItem(
    tabName = "drug_exposure_diagnostics",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "ded_cdm",
        label = "Database",
        choices = sort(unique(cohort_count$cdm_name)),
        selected = sort(unique(cohort_count$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "ded_cohort",
        label = "Ingredient",
        choices = sort(unique(cohort_count$cohort_name)),
        selected = sort(unique(cohort_count$cohort_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
  ),
  tags$style(HTML("
                  .tabbable > .nav > li > a {font-weight: bold; background-color: D3D4D8;  color:black}
                  ")),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Concepts in database",
      DT::dataTableOutput("dt_conceptSummary") %>%
        withSpinner()
    ),
    tabPanel(
      "Drug record durations",
      tags$hr(),
      radioGroupButtons(
        inputId = "ded_dur_byConcept",
        choices = c("Overall per ingredient", 
                    "By concept"),
        justified = TRUE
      ),
      tags$hr(),
      DT::dataTableOutput("dt_drugExposureDuration") %>%
        withSpinner()
    ),
    tabPanel(
      "Drug source concepts",
      DT::dataTableOutput("dt_drugSourceConceptsByConcept") %>%
        withSpinner()
    ),
    tabPanel(
      "Drug record missing values",
      tags$hr(),
      radioGroupButtons(
        inputId = "ded_miss_byConcept",
        choices = c("Overall per ingredient", 
                    "By concept"),
        justified = TRUE
      ),
      pickerInput(
        inputId = "ded_var",
        label = "Field in drug exposure table",
        choices = sort(unique(missingValues$variable)),
        selected = sort(unique(missingValues$variable)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      tags$hr(),
      DT::dataTableOutput("dt_missingValues") %>%
        withSpinner()
    )
  
  
  )
  ),
  
  # prevalence 
  # cohort diagnostics -----
  tabItem(
    tabName = "cohort_diagnostics",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "cd_cdm",
        label = "Database",
        choices = sort(unique(cohort_count$cdm_name)),
        selected = sort(unique(cohort_count$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "cd_cohort",
        label = "Cohort",
        choices = sort(unique(cohort_count$cohort_name)),
        selected = sort(unique(cohort_count$cohort_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tags$style(HTML("
                  .tabbable > .nav > li > a {font-weight: bold; background-color: D3D4D8;  color:black}
                  ")),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Cohort counts",
        tags$hr(),
        prettySwitch(
          inputId = "cd_cc_subjects",
          label = "Number of subjects",
          fill = TRUE, 
          value = TRUE
        ),
        prettySwitch(
          inputId = "cd_cc_records",
          label = "Number of records",
          fill = TRUE, 
          value = TRUE
        ),
        tags$hr(),
        DT::dataTableOutput("dt_cohort_count") %>% 
          withSpinner()
      ),
      tabPanel(
        "Index codes",
        tags$hr(),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cd_index_group_name",
            label = "Group name",
            choices = sort(unique(index_codes$group_name)),
            selected = sort(unique(index_codes$group_name)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cd_index_strata_name",
            label = "Strata name",
            choices = sort(unique(index_codes$strata_name)),
            selected = sort(unique(index_codes$strata_name)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        tags$hr(),
        DT::dataTableOutput("dt_index_codes") %>% 
          withSpinner()
      )
      # ,
      # tabPanel(
      #   "Cohort intersection",
      #   tags$hr(),
      #   DT::dataTableOutput("dt_cohort_intersection") %>% 
      #     withSpinner(),
      #   tags$h5("Note, for cohort intersection only the first entry per cohort per individual is considered.")
      # )
    )
  )
  ,
  
  ### incidence ----
  tabItem(
    tabName = "incidence",
    h3("Incidence estimates"),
    p("Incidence estimates are shown below, please select configuration to filter them:"),
    p("Database and study outcome"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_cdm_name",
        label = "CDM name",
        choices = unique(incidence$cdm_name),
        selected = unique(incidence$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_outcome_cohort_name",
        label = "Outcome name",
        choices = unique(incidence$outcome_cohort_name),
        selected = unique(incidence$outcome_cohort_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Denominator population settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_denominator_target_cohort_name",
        label = "Target cohort",
        choices = unique(incidence$denominator_target_cohort_name),
        selected = "General population",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_denominator_age_group",
        label = "Age group",
        choices = unique(incidence$denominator_age_group),
        selected = "0 to 150",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_denominator_sex",
        label = "Sex",
        choices = unique(incidence$denominator_sex),
        selected = "Both",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_denominator_days_prior_observation",
        label = "Days prior observation",
        choices = unique(incidence$denominator_days_prior_observation),
        selected = unique(incidence$denominator_days_prior_observation),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Analysis settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_analysis_outcome_washout",
        label = "Outcome washout",
        choices = unique(incidence$analysis_outcome_washout),
        selected = unique(incidence$analysis_outcome_washout),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_analysis_repeated_events",
        label = "Repeated events",
        choices = unique(incidence$analysis_repeated_events),
        selected = unique(incidence$analysis_repeated_events),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_analysis_complete_database_intervals",
        label = "Complete period",
        choices = unique(incidence$analysis_complete_database_intervals),
        selected = unique(incidence$analysis_complete_database_intervals),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Dates"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_analysis_interval",
        label = "Interval",
        choices = unique(incidence$analysis_interval),
        selected = "years",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "incidence_estimates_incidence_start_date",
        label = "Incidence start date",
        choices = as.character(unique(incidence$incidence_start_date)),
        selected = as.character(unique(incidence$incidence_start_date)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table of estimates",
        downloadButton("incidence_estimates_download_table", "Download current estimates"),
        DTOutput("incidence_estimates_table") %>% withSpinner()
      ),
      tabPanel(
        "Plot of estimates",
        p("Plotting options"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_plot_x",
            label = "x axis",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
            selected = "incidence_start_date",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_plot_facet",
            label = "Facet by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
            selected = c("outcome_cohort_name"),
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_plot_colour",
            label = "Colour by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
            selected = "cdm_name",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        plotlyOutput(
          "incidence_estimates_plot",
          height = "800px"
        ) %>%
          withSpinner(),
        h4("Download figure"),
        div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("incidence_estimates_download_height", "", 10, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("incidence_estimates_download_width", "", 20, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block; margin-right:",
          textInput("incidence_estimates_download_dpi", "", 300, width = "50px")
        ),
        downloadButton("incidence_estimates_download_plot", "Download plot")
      )
    )
  ),
  # prevalence ----
  tabItem(
    tabName = "prevalence",
    h3("Prevalence estimates"),
    p("Prevalence estimates are shown below, please select configuration to filter them:"),
    p("Database and study outcome"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_cdm_name",
        label = "CDM name",
        choices = unique(prevalence$cdm_name),
        selected = unique(prevalence$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_outcome_cohort_name",
        label = "Outcome name",
        choices = unique(prevalence$outcome_cohort_name),
        selected = unique(prevalence$outcome_cohort_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Denominator population settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_denominator_target_cohort_name",
        label = "Target cohort",
        choices = unique(prevalence$denominator_target_cohort_name),
        selected = "General population",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_denominator_age_group",
        label = "Age group",
        choices = unique(prevalence$denominator_age_group),
        selected = "0 to 150",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_denominator_sex",
        label = "Sex",
        choices = unique(prevalence$denominator_sex),
        selected = "Both",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_denominator_days_prior_observation",
        label = "Days prior observation",
        choices = unique(prevalence$denominator_days_prior_observation),
        selected = unique(prevalence$denominator_days_prior_observation),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Analysis settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_analysis_type",
        label = "Prevalence type",
        choices = unique(prevalence$analysis_type),
        selected = unique(prevalence$analysis_type),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_analysis_time_point",
        label = "Time point",
        choices = unique(prevalence$analysis_time_point),
        selected = unique(prevalence$analysis_time_point),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_analysis_complete_database_intervals",
        label = "Complete period",
        choices = unique(prevalence$analysis_complete_database_intervals),
        selected = unique(prevalence$analysis_complete_database_intervals),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_analysis_full_contribution",
        label = "Full contribution",
        choices = unique(prevalence$analysis_full_contribution),
        selected = unique(prevalence$analysis_full_contribution),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Dates"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_analysis_interval",
        label = "Interval",
        choices = unique(prevalence$analysis_interval),
        selected = "years",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "prevalence_estimates_prevalence_start_date",
        label = "Prevalence start date",
        choices = as.character(unique(prevalence$prevalence_start_date)),
        selected = as.character(unique(prevalence$prevalence_start_date)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table of estimates",
        downloadButton("prevalence_estimates_download_table", "Download current estimates"),
        DTOutput("prevalence_estimates_table") %>% withSpinner()
      ),
      tabPanel(
        "Plot of estimates",
        p("Plotting options"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_estimates_plot_x",
            label = "x axis",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "analysis_type", "analysis_time_point", 
                        "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"),
            selected = "prevalence_start_date",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_estimates_plot_facet",
            label = "Facet by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_type", "analysis_outcome_lookback_days", "analysis_time_point", "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"),
            selected = "outcome_cohort_name",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_estimates_plot_colour",
            label = "Colour by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_type", "analysis_outcome_lookback_days", "analysis_time_point", "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"),
            selected = "cdm_name",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        plotlyOutput(
          "prevalence_estimates_plot",
          height = "800px"
        ) %>%
          withSpinner(),
        h4("Download figure"),
        div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("prevalence_estimates_download_height", "", 10, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("prevalence_estimates_download_width", "", 20, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block; margin-right:",
          textInput("prevalence_estimates_download_dpi", "", 300, width = "50px")
        ),
        downloadButton("prevalence_estimates_download_plot", "Download plot")
      )
    )
  ),
  # cohort characteritics -------
  tabItem(
    tabName = "chars",
    tags$hr(),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "chars_cdm",
        label = "Database",
        choices = sort(unique(patient_characteristics$cdm_name)),
        selected = sort(unique(patient_characteristics$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "chars_cohort",
        label = "Cohort",
        choices = sort(unique(patient_characteristics$group_level)),
        selected = sort(unique(patient_characteristics$group_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
      tags$hr(),
      gt_output("gt_patient_characteristics") %>% 
        withSpinner()
    ),
  # lsc ------
  tabItem(
    tabName = "lsc",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "lsc_cdm",
        label = "Database",
        choices = sort(unique(large_scale_characteristics$cdm_name)),
        selected = sort(unique(large_scale_characteristics$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "lsc_cohort",
        label = "Cohort",
        choices = sort(unique(large_scale_characteristics$group_level)),
        selected = sort(unique(large_scale_characteristics$group_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "lsc_domain",
        label = "Domain",
        choices = sort(unique(large_scale_characteristics$table_name)),
        selected = sort(unique(large_scale_characteristics$table_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "lsc_time_window",
        label = "Time window",
        choices = sort(unique(large_scale_characteristics$variable_level)),
        selected = sort(unique(large_scale_characteristics$variable_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tags$hr(),
    DT::dataTableOutput("dt_large_scale_characteristics") %>% 
      withSpinner()
  ),
  # indication ------
  tabItem(
    tabName = "indication",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "indication_cdm",
        label = "Database",
        choices = sort(unique(indication$cdm_name)),
        selected = sort(unique(indication$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "indication_cohort",
        label = "Cohort",
        choices = sort(unique(indication$group_level)),
        selected = sort(unique(indication$group_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "indication_time_window",
        label = "Time window",
        choices = sort(unique(indication$variable)),
        selected = sort(unique(indication$variable)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "indication_indication",
        label = "Indication",
        choices = sort(unique(indication$variable_level)),
        selected = sort(unique(indication$variable_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tags$hr(),
    DT::dataTableOutput("dt_indication") %>% 
      withSpinner()
  ),
  
  # dus -------
  tabItem(
    tabName = "dus",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "dus_cdm",
        label = "Database",
        choices = sort(unique(dus_summary$cdm_name)),
        selected = sort(unique(dus_summary$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "dus_cohort",
        label = "Cohort",
        choices = sort(unique(dus_summary$group_level)),
        selected = sort(unique(dus_summary$group_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tags$style(HTML("
                  .tabbable > .nav > li > a {font-weight: bold; background-color: D3D4D8;  color:black}
                  ")),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Duration",
        tags$hr(),
    DT::dataTableOutput("dt_dus_duration") %>% 
      withSpinner()
    ),
    tabPanel(
      "Initial daily dose (milligram)",
      tags$hr(),
      DT::dataTableOutput("dt_dus_initial_dd") %>% 
        withSpinner()
    ),
    tabPanel(
      "Cumulative dose (milligram)",
      tags$hr(),
      DT::dataTableOutput("dt_dus_cumulative_dose") %>% 
        withSpinner()
    )
    )
  )
  
  
  # end -----
    )
  )
)


