# Drug Exposure Diagnostics App

A shiny application to view the results of the [DrugExposureDiagnostics](https://github.com/darwin-eu/DrugExposureDiagnostics)

### Requirements

* a directory containing the diagnostics result from the DrugExposureDiagnostics package
* make sure the DATA_DIRECTORY variable in global.R points to your data directory. 
The results from each database should be in a separate folder below this directory. 
Note: if you just want to run the app with some example data, please change the DATA_DIRECTORY variable to "exampleData".
This data is from diagnostics about "acetaminophen" in the eunomia and synthea test database.
* an environment with R, Rstudio and packages installed. To setup your environment with the right versions of packages use renv: 
``` r 
install.packages("renv") 
renv::restore()
```

### Run

The app can be run by opening either server.R, ui.R or global.R and clicking on "Run App" in the top right corner.
