# fluroquinolones ----
cli:: cli_text("Getting codes for fluroquinolones")
fluoroquinolones <- getDrugIngredientCodes(cdm = cdm,
                                           name = c("ciprofloxacin",
                                                    "delafloxacin",
                                                    "moxifloxacin",
                                                    "ofloxacin",
                                                    "levofloxacin",
                                                    "norfloxacin"))

fl_counts <- list()
cli::cli_progress_bar("Getting counts of fluoroquinolones",
                      total = length(fluoroquinolones))
for(i in seq_along(fluoroquinolones)){
  cli::cli_progress_update()
  fl_counts[[i]] <- summariseCodeUse(x = fluoroquinolones[[i]],
                                     cdm = cdm,
                                     byConcept = TRUE,
                                     byYear = TRUE,
                                     bySex = TRUE,
                                     ageGroup = list(c(0,17),
                                                     c(18,150))) %>%
    mutate(group = names(fluoroquinolones)[i])
}
cli::cli_progress_done()
fl_counts <- bind_rows(fl_counts)

# rectal prolapse ----
cli:: cli_text("Getting counts for rectal prolapse")
# getCandidateCodes(cdm = cdm,
#                   keywords = c("rectal prolapse"),
#                   searchInSynonyms = TRUE)
rp_counts <- summariseCodeUse(x = c(81336,
                                    4055197,
                                    4071880,
                                    4096338,
                                    4183172,
                                    4325606,
                                    36716683,
                                    44791566,
                                    4006305,
                                    4006985,
                                    36676438),
                              cdm = cdm,
                              byConcept = TRUE,
                              byYear = TRUE,
                              bySex = FALSE,
                              ageGroup = NULL) %>%
  mutate(group = "rectal prolapse")

# rectopexy -----
cli:: cli_text("Getting counts for rectopexy")
# getCandidateCodes(cdm,
#                   c("rectopexy", "rectum repair",
#                     "fixation of rectum", "prolapse of rectum"),
#                   domains = c("procedure"))
rec_counts <- summariseCodeUse(x = c(4125300,
                                    4139161,4144720,37310724,37310731,40480623,2003011,4077946,
                                    4147820,4230535,4233818,40479990,4123418,4128864,4139160,4142370,
                                    4172220,4209077,4257890,37203797,37203798,37203799,46270687,4080043,
                                    4126235,4144719,4195632,42873100,4010018,4125301,2109162,2109163,
                                    2109211,2109212,2109217,2109221,2109278,2109279,2109280,2109281,
                                    2109282,2110109,2110110,2110111,2110113,2110114,2110187,2109214,
                                    2109215,2109216,4135951,4134717,4290511,4146637,4141257,44783696,
                                    3655997,4332008,4218743,2109220,4002561,4147611,2110127,4069010,
                                    2110129,4146636,2109223,4146035,4018134,4250788,44805432,4070766,
                                    4057407,4227434,2109222,4045946,2109213,4123405,4018281,4066875,
                                    4284692,4017608,4326580,4275570,43531107,4029972,4172800,4245100,
                                    4123417,4216505,4042907,2109069,4069845,2004604,4172146,4124301,
                                    2109219,4165586,4144205,4229775,4067346,4224344,4018283,4141259,
                                    4026223,4146034,4146028,4128866,2110128,40491978,4067445,4001368,
                                    2109071,2109288,43531108,4167524,2003007,2109273,4125303,2109045,
                                    2109272,4167979,4068417,4084297,4221999,2109068,4201274,37016994,
                                    4177704,4033545,2110130,4146631,2109048,4121238,4221568,2109155,
                                    2003010,2109218,4123413,2109067,4252402,4311017,4202217,4262950,
                                    2109046,4295431,2109224,2109209,4250893,2109208,4013041,4104637,
                                    4329865),
                               cdm = cdm,
                               byConcept = TRUE,
                               byYear = TRUE,
                               bySex = FALSE,
                               ageGroup = NULL) %>%
  mutate(group = "rectopexy")



# report ----
rmarkdown::render('FeasibilityReport.Rmd',
                  params = list(cdm = cdm,
                                fl_counts = fl_counts,
                                rec_counts = rec_counts,
                                set_title = paste0("Study feasibility: ", database_name) ),
                  output_file = paste0("Feasibility_", database_name, '.docx'))
