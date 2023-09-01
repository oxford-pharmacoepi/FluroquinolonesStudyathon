# StudyathonFeasibilityMHRA

## Description
The code here runs feasibility queries on:
- fluroquinolones (ciprofloxacin, delafloxacin, moxifloxacin, ofloxacin, levofloxacin, norfloxacin)
- condition occurrence of rectal prolapse
- procedure of rectopexy
- device exposure of mesh

A word document is generated summarising record and person counts (overall, by concept id, and by year). Any count less that 5 is suppressed.

## Instructions for data partners
The only file you should need to interact with is the CodeToRun.R where you will need to specify your database connection details using DBI (https://dbi.r-dbi.org/reference/dbconnect) and then create a reference to the OMOP CDM using CDMConnector (https://darwin-eu.github.io/CDMConnector/). After having done this then running the last line of code will run the counts and generate a word document with the results to share.  
