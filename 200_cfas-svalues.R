rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

load(here::here(rds_filepath, "010_tidy-data.Rdata"))

#### Memory

id_key <- tidied %>% 
  dplyr::arrange(ADAMSSID) %>% 
  dplyr::mutate(mplusid = row_number()) %>% 
  dplyr::select(mplusid, ADAMSSID)

mem_data <- tidied %>% 
  dplyr::arrange(ADAMSSID) %>% 
  dplyr::mutate(mplusid = row_number()) %>% 
  dplyr::select(mplusid,
                vdmde1, vdmde2, vdmde3, vdmde4, vdmre1cat, vdmde6)

psych::describe(mem_data)

mem_model <- mplusObject(
  MODEL = "
  
     mem BY vdmde1@2.42053;
     mem BY vdmde2@3.78470;
     mem BY vdmde3@1.43515;
     mem BY vdmde4@2.66786;
     mem BY vdmre1cat@1.81536;
     mem BY vdmde6@3.75506;
     lm BY vdmde2@2.07174 (19);
     lm BY vdmde6@2.07174 (19);

     mem WITH lm@0;

     [ vdmde1@3.67561 ];
     [ vdmde2@6.86468 ];
     [ vdmde4@5.13934 ];
     [ vdmde6@6.98423 ];

     [ vdmde3$1@-0.80624 ];
     [ vdmde3$2@-0.07685 ];
     [ vdmde3$3@1.10999 ];
     [ vdmre1cat$1@-2.70659 ];
     [ vdmre1cat$2@-2.54413 ];
     [ vdmre1cat$3@-1.92582 ];
     [ vdmre1cat$4@-1.55445 ];
     [ vdmre1cat$5@-0.97542 ];
     [ vdmre1cat$6@-0.72481 ];
     [ vdmre1cat$7@-0.30447 ];
     [ vdmre1cat$8@0.17001 ];
     [ vdmre1cat$9@1.03680 ];

     vdmde1@1.56337;
     vdmde2@5.07884;
     vdmde4@5.12281;
     vdmde6@4.44574;
     mem@1;
     lm@1;
  
  ",
  usevariables = colnames(mem_data),
  VARIABLE = "idvariable = mplusid;
              CATEGORICAL ARE vdmde3 vdmre1cat;",
  OUTPUT = "TECH4 STANDARDIZED;",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = mem_data,
  SAVEDATA = "SAVE = fscores;
              file is memfscores.dat;")

mem <- mplusModeler(mem_model, 
                    modelout = "mem.inp", run = TRUE)

#### Executive Functioning

exf_data <- tidied %>% 
  dplyr::arrange(ADAMSSID) %>% 
  dplyr::mutate(mplusid = row_number()) %>% 
  dplyr::select(mplusid, vdexf8, vdexf9, vdasp1, vdasp3)

psych::describe(exf_data)

exf_model <- mplusObject(
  MODEL = "
  
   exf BY vdexf8@1.80346;
     exf BY vdexf9@2.01672;
     exf BY vdasp1@7.92516;
     exf BY vdasp3@1.42827;

     [ vdexf8@7.55930 ];
     [ vdexf9@4.13425 ];
     [ vdasp1@24.63652 ];

     [ vdasp3$1@-1.60488 ];
     [ vdasp3$2@-1.07908 ];
     [ vdasp3$3@-0.35482 ];
     [ vdasp3$4@0.14121 ];
     [ vdasp3$5@0.23939 ];

     vdexf8@2.31757;
     vdexf9@1.57502;
     vdasp1@90.70109;
     exf@1;

  
  
  ",
  usevariables = colnames(exf_data),
  VARIABLE = "idvariable = mplusid;
              CATEGORICAL = vdasp3;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES MODINDICES(ALL);",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = exf_data,
  SAVEDATA = "SAVE = fscores;
              file is exffscores.dat;")

exf <- mplusModeler(exf_model, 
                    modelout = "exf.inp", run = TRUE)

#### Language

lfl_data <- tidied %>% 
  dplyr::arrange(ADAMSSID) %>% 
  dplyr::mutate(mplusid = row_number()) %>% 
  dplyr::select(mplusid,
                vdlfl1, vdlfl2, vdlfl3, vdlfl4, vdlfl5, vdlfl7, vdlfl8)

psych::describe(lfl_data)

lfl_model <- mplusObject(
  MODEL = "
  
   lfl BY vdlfl1@4.24201;
     lfl BY vdlfl2@2.17285;
     lfl BY vdlfl3@1.38471;
     lfl BY vdlfl4@0.88904;
     lfl BY vdlfl5@0.82570;
     lfl BY vdlfl7@2.58749;
     lfl BY vdlfl8@8.41852;

     [ vdlfl1@11.74046 ];
     [ vdlfl7@11.75783 ];
     [ vdlfl8@23.39673 ];

     [ vdlfl2$1@-2.95110 ];
     [ vdlfl2$2@-0.76952 ];
     [ vdlfl3$1@-3.16407 ];
     [ vdlfl4$1@-1.26859 ];
     [ vdlfl5$1@-1.62466 ];
     [ vdlfl5$2@-0.88426 ];
     [ vdlfl5$3@0.19056 ];

     vdlfl1@13.07615;
     vdlfl7@3.81561;
     vdlfl8@86.26258;
     lfl@1;
  
  ",
  usevariables = colnames(lfl_data),
  VARIABLE = "idvariable = mplusid;
              CATEGORICAL ARE vdlfl2 vdlfl3 vdlfl4 vdlfl5;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES MODINDICES(ALL);",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = lfl_data,
  SAVEDATA = "SAVE = fscores;
              file is lflfscores.dat;")

lfl <- mplusModeler(lfl_model, 
                    modelout = "lfl.inp", run = TRUE)

# Make fscores dataset

memfscore <- mem$results$savedata %>% 
  dplyr::select(MPLUSID, MEM) %>% 
  dplyr::rename(mplusid = MPLUSID)

exffscore <- exf$results$savedata %>% 
  dplyr::select(MPLUSID, EXF) %>% 
  dplyr::rename(mplusid = MPLUSID)

lflfscore <- lfl$results$savedata %>% 
  dplyr::select(MPLUSID, LFL) %>% 
  dplyr::rename(mplusid = MPLUSID)

fscores <- id_key %>% 
  dplyr::left_join(memfscore, by = "mplusid") %>% 
  dplyr::left_join(exffscore, by = "mplusid") %>% 
  dplyr::left_join(lflfscore, by = "mplusid")

readr::write_rds(fscores, here::here(paste0(rds_filepath, "fscores.rds")))
