library(DBI)
library(CDMConnector)
library(CodelistGenerator)
library(dplyr)
library(here)

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
                  writeSchema = "results")

drug_names <- data.frame(concept_name = c("ciprofloxacin",
                "delafloxacin",
                "moxifloxacin",
                "ofloxacin",
                "levofloxacin",
                "norfloxacin"))

drug_concepts <- cdm$concept %>%
  mutate(concept_name = tolower(concept_name)) %>%
  filter(concept_name %in% local(drug_names$concept_name)) %>%
  filter(concept_class_id=="Ingredient")    %>%
  filter(standard_concept =="S") %>%
  collect()


drug_concepts <- drug_names %>%
  left_join(drug_concepts,
            by = "concept_name") %>%
  select("concept_name", "concept_id")

write.csv(drug_concepts,
          file = here("drug_concepts", "drug_concepts.csv"),
          row.names = FALSE)
