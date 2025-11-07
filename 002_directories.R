user <- "Emma"
if (user == "Emma"){
  if (Sys.info()["sysname"] == "Windows") {
    data_filepath <- "C:\\Users\\emmanich\\P2AGING Dropbox\\Emma Nichols\\projects\\hcapalgorithm_adams\\data\\source\\"
    rds_filepath <- "C:\\Users\\emmanich\\P2AGING Dropbox\\Emma Nichols\\projects\\hcapalgorithm_adams\\data\\derived\\"
    code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
    images_filepath <- "C:\\Users\\emmanich\\P2AGING Dropbox\\Emma Nichols\\projects\\hcapalgorithm_adams\\plots\\"
  } else {
    data_filepath <- "/Users/emmanich/P2AGING Dropbox/Emma Nichols/projects/hcapalgorithm_adams/data/source/"
    rds_filepath <- "/Users/emmanich/P2AGING Dropbox/Emma Nichols/projects/hcapalgorithm_adams/data/derived/"
    images_filepath <- "/Users/emmanich/P2AGING Dropbox/Emma Nichols/projects/hcapalgorithm_adams/plots/"
    code_filepath <- "/Users/emmanich/code/ADAMS-HCAP-ALGO/"
  }
} else if (user == "zkunicki"){
  data_filepath <- "R:\\STUDIES\\HRSADAMS\\data\\SOURCE"
  rds_filepath <- "C:\\Users\\zkunicki\\OneDrive - Brown University\\Documents\\HomeMigration\\Research\\HRSADAMS\\PROJECTS\\ADAMS-HCAP-ALGO\\Images"
  images_path <- "C:\\Users\\zkunicki\\OneDrive - Brown University\\Documents\\HomeMigration\\Research\\HRSADAMS\\PROJECTS\\ADAMS-HCAP-ALGO\\RDS"
  code_filepath <- ""
}





