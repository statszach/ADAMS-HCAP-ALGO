rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"
if (Sys.info()["sysname"] == "Windows") {
    code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
} else {
    code_filepath <- "/Users/emmanich/code/ADAMS-HCAP-ALGO/"
}
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

tidied <- readr::read_rds(here::here(rds_filepath, "010_tidy-data.rds"))

#### Memory

mem_data <- tidied %>% 
  dplyr::select(ADAMSSID,
                vdmde1, vdmde2, vdmde3, vdmde4, vdmre1cat, vdmde6)

psych::describe(mem_data)

mem_model <- mplusObject(
  MODEL = "
  
  mem BY vdmde1* vdmde2 vdmde3 vdmde4 vdmre1cat vdmde6;
  mem@1;
  
  lm BY vdmde2* (1);
  lm BY vdmde6* (1);
  lm@1;
  
  mem with lm@0;
  
  ",
  usevariables = colnames(mem_data),
  VARIABLE = "idvariable = ADAMSSID;
              CATEGORICAL ARE vdmde3 vdmre1cat;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES MODINDICES(ALL);",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = mem_data)

mem <- mplusModeler(mem_model, 
                       modelout = "mem.inp", run = TRUE)

#### Executive Functioning

exf_data <- tidied %>% 
  dplyr::select(ADAMSSID, vdexf8, vdexf9, vdasp1, vdasp3)

psych::describe(exf_data)

exf_model <- mplusObject(
  MODEL = "
  
  exf BY  vdexf8* vdexf9 vdasp1 vdasp3;
  exf@1;
  
  
  ",
  usevariables = colnames(exf_data),
  VARIABLE = "idvariable = ADAMSSID;
              CATEGORICAL = vdasp3;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES MODINDICES(ALL);",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = exf_data)

exf <- mplusModeler(exf_model, 
                    modelout = "exf.inp", run = TRUE)

#### Language

lfl_data <- tidied %>% 
  dplyr::select(ADAMSSID,
                vdlfl1, vdlfl2, vdlfl3, vdlfl4, vdlfl5, vdlfl7, vdlfl8)

psych::describe(lfl_data)

lfl_model <- mplusObject(
  MODEL = "
  
  lfl BY vdlfl1* vdlfl2 vdlfl3 vdlfl4 vdlfl5 vdlfl7 vdlfl8;
  lfl@1;
  
  ",
  usevariables = colnames(lfl_data),
  VARIABLE = "idvariable = ADAMSSID;
              CATEGORICAL ARE vdlfl2 vdlfl3 vdlfl4 vdlfl5;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES MODINDICES(ALL);",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = lfl_data)

lfl <- mplusModeler(lfl_model, 
                    modelout = "lfl.inp", run = TRUE)
