# Fluroquinolones Studyathon 2023

|<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- Analytics use case(s): **Characterization**
- Study type: 
- Tags: 
- Study lead: 
- Study lead forums tag: 
- Study start date: 
- Study end date: 
- Protocol: 
- Publications: 
- Results explorer:

## Overview
This repository contains code for a drug utilisation study focused on the use of systemic fluoroquinolones in primary care and hospital settings in the UK

The repo is organised in the following manner:  
- Documents: This folder contains documents relevant to the study 
- Diagnostics: This folder containers code to run study-specific data checks
- Study: This folder contains code to run the study


## Data partner instructions

To start download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 

### Task 1: Run drug exposure diagnostics
1) Go to folder "Diagnostics/DrugExposureDiagnostics" and open the project <i>DrugExposureDiagnostics.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
2) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on.
3) After running you should then have a zip folder with results to share in your results folder. To view the results, go to "Diagnostics/1b_fluroquinolones_diagnostics_shiny/DrugExposureDiagnosticsShiny.Rproj", add unzipped results to the data folder,  and launch the app from ui.R

