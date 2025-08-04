rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "zkunicki"
source(here::here("001_libraries.R"))
source(here::here("002_directories.R"))

## AN_R has the neuroexam 

ADAMS1AN_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AN_R.sav"))

## Loading tracker filer for demographics and weights

ADAMS1TRK_R <- haven::read_dta(fs::path(data_filepath, "ADAMS1TRK_R.dta"))

## AG_R has caregiver info

ADAMS1AG_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AG_R.sav"))

save.image(here::here(rds_path, "003_load-data.Rdata"))