rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

## AN_R has the neuroexam 

ADAMS1AN_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AN_R.sav"))

## Loading tracker filer for demographics and weights

ADAMS1TRK_R <- haven::read_dta(fs::path(data_filepath, "ADAMS1TRK_R.dta"))

## AG_R has caregiver info

ADAMS1AG_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AG_R.sav"))

## AD_R has the blessed

ADAMS1AD_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AD_R.sav"))

save.image(here::here(rds_filepath, "003_load-data.Rdata"))
