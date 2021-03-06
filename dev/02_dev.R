# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "stats" )
usethis::use_package( "utils" )
usethis::use_package( "plotly" )
usethis::use_package( "renv" )
usethis::use_package( "DT" )
usethis::use_package("shinycssloaders")
usethis::use_package("dplyr")
usethis::use_pipe()


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "choose_referencedata" ) # Name of the module
golem::add_module( name = "load_fromweb" ) # Name of the module
golem::add_module( name = "choose_userdata" ) # Name of the module 
golem::add_module( name = "analysis" )
golem::add_module( name = "analysis2" )
golem::add_module( name = "analysis_fct_data" )
golem::add_module( name = "analysis2_fct_data" )


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )
usethis::use_test("missing_and_duplicates")
usethis::use_test("matching_regcodes_names")

# Documentation

## Vignette ----
usethis::use_vignette("TheApp")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

