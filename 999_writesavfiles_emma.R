
if (Sys.info()["sysname"] == "Windows") {
    data_dir <- "C:\\Users\\emmanich\\P2AGING Dropbox\\Emma Nichols\\projects\\hcapalgorithm_adams\\data\\source\\"
} else {
    data_dir <- "/Users/emmanich/P2AGING Dropbox/Emma Nichols/projects/hcapalgorithm_adams/data/source/"
}


save_new <- function(wave = "A", letter){
    name <- paste0("ADAMS1", wave, letter, "_R")
    file <- SAScii::read.SAScii(fn = here::here(paste0(data_dir, name, ".da")),
                              sas_ri = here::here(paste0(data_dir, name, ".sas")),
                              beginline = 1)
    haven::write_sav(file, here::here(paste0(data_dir, name, ".sav")))
}

lapply(LETTERS[c(10, 13:14)], save_new)
lapply(LETTERS[2:3], function(x) save_new(wave = x, letter = "D"))
