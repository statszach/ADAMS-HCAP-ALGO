rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"
if (Sys.info()["sysname"] == "Windows") {
    code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
} else {
    code_filepath <- "/Users/emmanich/code/ADAMS-HCAP-ALGO/"
}
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

## AN_R has the neuroexam 

ADAMS1AN_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AN_R.sav"))

## Loading tracker filer for demographics and weights

ADAMS1TRK_R <- haven::read_dta(fs::path(data_filepath, "ADAMS1TRK_R.dta"))

## AG_R has caregiver info

ADAMS1AG_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AG_R.sav"))

## AD_R has the blessed and diagnoses

ADAMS1AD_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1AD_R.sav"))

## diagnoses for follow-up waves 

ADAMS1BD_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1BD_R.sav"))
ADAMS1CD_R <- haven::read_sav(fs::path(data_filepath, "ADAMS1CD_R.sav"))

## rand 2000 for self-rated memory 

RAND2000 <- haven::read_dta(fs::path(data_filepath, "h00f1d.dta"))

## rand 2002 for self-rated memory 

RAND2002 <- haven::read_dta(fs::path(data_filepath, "h02f2c.dta"))

## harmonized hrs for adls/iadls

GATEWAY <- haven::read_dta(fs::path(data_filepath, "H_HRS_d.dta"))

save.image(here::here(rds_filepath, "003_load-data.Rdata"))