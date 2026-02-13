rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"
if (Sys.info()["sysname"] == "Windows") {
    code_filepath <- "C:/Users/emmanich/code/ADAMS-HCAP-ALGO/"
} else {
    code_filepath <- "/Users/emmanich/code/ADAMS-HCAP-ALGO/"
}
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

load(here::here(rds_filepath, "003_load-data.Rdata"))

#### Orientation

# vdori1 is the sum of the first 10 MMSE items.
# WE DO NOT KNOW THE TEXT OF THE FIRST 10 MMSE ITEMS FROM THE DOCUMENTATION.
# We also sum over any missing items, treating them as if they were missing.

vdori1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE1:ANMSE10) %>% 
  dplyr::mutate(across(ANMSE1:ANMSE10, ~na_if(., 97))) %>% 
  rowwise() %>%
  mutate(vdori1 = sum(c_across(ANMSE1:ANMSE10), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(ADAMSSID, vdori1)

#### Memory

# vdmde1 is just a renaming of ANDELCOR, which I believe
# is the CERAD word list delayed

vdmde1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANDELCOR) %>% 
  dplyr::mutate(ANDELCOR = na_if(ANDELCOR, 97)) %>% 
  dplyr::rename(vdmde1 = ANDELCOR)

# vdmde2 is the number correct on the WMS-IV Logical Memory 1 delayed
# it's a renaming of ANWM1A

vdmde2 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANWM1A) %>% 
  dplyr::mutate(ANWM1A = na_if(ANWM1A, 97)) %>% 
  dplyr::rename(vdmde2 = ANWM1A)

# vdmde3 is the MMSE 3 word delayed recall
# I believe this is the sum of items 13, 14, and 15 because
# the MMSE asks for delayed recall after the spell world backwards
# item, which is definitely item 12.

# note that Jones (2025) treats this as a three category variable
# due to so few participants scoring 0. we do not have that issue
# so I did not make that recoding decision.

vdmde3 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE13:ANMSE15) %>% 
  dplyr::mutate(across(ANMSE13:ANMSE15, ~na_if(., 97))) %>% 
  rowwise() %>%
  mutate(vdmde3 = sum(c_across(ANMSE13:ANMSE15), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(ADAMSSID, vdmde3)

# vdmde4 is the CERAD constructional praxis delayed
# it is a renaming of ANDCPTOT

vdmde4 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANDCPTOT) %>% 
  dplyr::mutate(ANDCPTOT = na_if(ANDCPTOT, 97)) %>% 
  dplyr::rename(vdmde4 = ANDCPTOT)

# vdmde6 is the number correct on the WMS-IV Logical Memory 2 delayed
# it's a renaming of ANWM1B.
# this test is ONLY in ADAMS, not in HCAP.

vdmde6 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANWM1B) %>% 
  dplyr::mutate(ANWM1B = na_if(ANWM1B, 97)) %>% 
  dplyr::rename(vdmde6 = ANWM1B)

# vdmre1 is the CERAD word list recognition
# it is the sum of ANRECYES and ANRECNO
# I am guessing we will need to bin this item based on it's
# distribution

vdmre1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANRECYES:ANRECNO) %>% 
  dplyr::mutate(across(ANRECYES:ANRECNO, ~na_if(., 97))) %>% 
  rowwise() %>%
  mutate(vdmre1 = sum(c_across(ANRECYES:ANRECNO), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(ADAMSSID, vdmre1) %>% 
  dplyr::mutate(vdmre1cat = dplyr::case_when(
    vdmre1 == 0 ~ 0,
    between(vdmre1, 7, 9) ~ 1,
    between(vdmre1, 10, 11) ~ 2,
    between(vdmre1, 12, 13) ~ 3,
    between(vdmre1, 14, 15) ~ 4,
    vdmre1 == 16 ~ 5,
    vdmre1 == 17 ~ 6,
    vdmre1 == 18 ~ 7,
    vdmre1 == 19 ~ 8,
    vdmre1 == 20 ~ 9))

#### Executive Functioning

# vdexf2 is Trails B. we score it through the formula
# 1 - (log(ANTMBSEC) - log(300).

vdexf2 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANTMBSEC) %>%
  dplyr::mutate(vdexf2 = dplyr::case_when(
    ANTMBSEC < 994 ~ 1 - (log(ANTMBSEC)/log(300)),
    ANTMBSEC > 994 ~ NA_real_
  )) %>% 
  dplyr::select(ADAMSSID, vdexf2)

# vdasp1 is the Symbol Digit Modalities Test.
# it is simply a renaming of ANSDMTOT

vdasp1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANSDMTOT) %>% 
  dplyr::mutate(ANSDMTOT = na_if(ANSDMTOT, 97)) %>% 
  dplyr::rename(vdasp1 = ANSDMTOT)


# vdasp2 is Trails A. we score it through the same formula
# as trails B.

vdasp2 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANTMASEC) %>%
  dplyr::mutate(vdasp2 = dplyr::case_when(
    ANTMASEC < 994 ~ 1 - (log(ANTMASEC)/log(300)),
    ANTMASEC > 994 ~ NA_real_
  )) %>% 
  dplyr::select(ADAMSSID, vdasp2)

# vdasp3 is the MMSE item spell world backwards.
# it is a rename of ANMSE12

vdasp3 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE12) %>% 
  dplyr::mutate(ANMSE12 = na_if(ANMSE12, 97)) %>% 
  dplyr::rename(vdasp3 = ANMSE12)

# vdexf8 is digit span forwards. it is an ADAMS test
# not given in HCAP. it is a renaming of ANDSSFT

vdexf8 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANDSSFT) %>% 
  dplyr::mutate(ANDSSFT = na_if(ANDSSFT, 97)) %>% 
  dplyr::rename(vdexf8 = ANDSSFT)

# vdexf9 is digit span backwards it is an ADAMS test
# not given in HCAP. it is a renaming of ANDSSBT

vdexf9 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANDSSBT) %>% 
  dplyr::mutate(ANDSSBT = na_if(ANDSSBT, 97)) %>% 
  dplyr::rename(vdexf9 = ANDSSBT)

#### Language

# vdlfl1 is the animal naming test. it is a
# renaming of ANAFTOT

vdlfl1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANAFTOT) %>% 
  dplyr::mutate(ANAFTOT = na_if(ANAFTOT, 97)) %>% 
  dplyr::rename(vdlfl1 = ANAFTOT)

# vdlfl2 is naming 2 items from the TICS. it is the sum of
# ANSCISOR and ANCACTUS

vdlfl2 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANSCISOR:ANCACTUS) %>% 
  dplyr::mutate(across(ANSCISOR:ANCACTUS, ~ if_else(.x %in% c(97, 98, 99), NA, .x))) %>% 
  rowwise() %>%
  mutate(vdlfl2 = sum(c_across(ANSCISOR:ANCACTUS), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(ADAMSSID, vdlfl2)

# vdlfl3 is naming 2 items from the MMSE. it should be
# the sum of ANMSE16 and ANMSE17, although we cannot be certain.
# scores of 0 or 1 are collapsed to 0 with this item.
# tactile stimuli was coded as correct

vdlfl3 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE16:ANMSE17) %>% 
  dplyr::mutate(ANMSE16 = dplyr::case_when(ANMSE16 == 0 ~ 0,
                                           ANMSE16 == 1 ~ 1,
                                           ANMSE16 == 2 ~ 1,
                                           ANMSE16 == 97 ~ NA_real_),
                ANMSE17 = dplyr::case_when(ANMSE17 == 0 ~ 0,
                                           ANMSE17 == 1 ~ 1,
                                           ANMSE17 == 2 ~ 1,
                                           ANMSE17 == 97 ~ NA_real_),
                sum = ANMSE16 + ANMSE17,
                vdlfl3 = dplyr::if_else(sum == 2, 1, 0)) %>% 
  dplyr::select(ADAMSSID, vdlfl3)

# vdlfl4 is MMSE write a sentence. it is ANMSE21. writing name is coded as correct

vdlfl4 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE21) %>% 
  dplyr::mutate(vdlfl4 = dplyr::case_when(ANMSE21 == 0 ~ 0,
                                          ANMSE21 == 1 ~ 1,
                                          ANMSE21 == 2 ~ 1,
                                          ANMSE21 == 97 ~ NA_real_)) %>% 
  dplyr::select(ADAMSSID, vdlfl4)

# vdlfl5 is MMSE read and follow command. we assume is ANMSE20F, L, R.
# Jones (2025) recodes this into a two category variable, but we may not have this issue

vdlfl5 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSE20F:ANMSE20R) %>% 
  dplyr::mutate(across(ANMSE20F:ANMSE20R, ~na_if(., 97))) %>% 
  rowwise() %>%
  mutate(vdlfl5 = sum(c_across(ANMSE20F:ANMSE20R), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(ADAMSSID, vdlfl5)

# vdlfl7 is the boston naming test. it is an ADAMS test not given in HCAP
# it is a rename of ANBNTTOT.
# we may have to bin this item too.

vdlfl7 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANBNTTOT) %>% 
  dplyr::mutate(ANBNTTOT = na_if(ANBNTTOT, 97)) %>% 
  dplyr::rename(vdlfl7 = ANBNTTOT)

# vdlfl8 is the controlled oral word association. it is an ADAMS test not given in HCAP
# it is a rename of ANCOWATO

vdlfl8 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANCOWATO) %>% 
  dplyr::mutate(ANCOWATO = na_if(ANCOWATO, 97)) %>% 
  dplyr::rename(vdlfl8 = ANCOWATO)

#### Visiospatial

# vdvis1 is the constructional praxis. it is a rename of ANCPTOT

vdvis1 <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANCPTOT) %>% 
  dplyr::mutate(ANCPTOT = na_if(ANCPTOT, 97)) %>% 
  dplyr::rename(vdvis1 = ANCPTOT)

#### Functioning

# Caregiver

iqcode <- ADAMS1AG_R %>% 
  dplyr::select(ADAMSSID, AGQ14:AGQ29) %>% 
  dplyr::mutate(across(contains("AGQ"), ~ replace(.x, .x %in% c(7, 8), NA))) %>%
  dplyr::mutate(iqcode_mean = rowMeans(across(AGQ14:AGQ29), na.rm = TRUE)) %>% 
  dplyr::select(ADAMSSID, iqcode_mean)

# clinician

# Define the item pairs
letters_vec <- LETTERS[1:8]  # A through H
items <- paste0("ADBL1", letters_vec)
responses <- paste0("ADBL1", letters_vec, "R")

# Generate blessed1 to blessed8
blessed <- ADAMS1AD_R %>%
  dplyr::select(ADAMSSID, ADBL1A:ADBL1HR) %>% 
  dplyr::mutate(
    across(
      .cols = everything(),  # placeholder so mutate works
      .fns = ~.,
      .names = "{.col}"      # no-op, we add below
    )
  ) %>%
  {
    reduce2(
      items, responses, .init = ., .f = function(d, itm, rsp) {
        blessed_name <- paste0("blessed", substr(itm, 6, 6))
        d %>%
          mutate(
            !!blessed_name := case_when(
              !!sym(itm) == 0 ~ 0,
              !!sym(itm) > 0 & !!sym(rsp) == 0 ~ 0,
              !!sym(itm) > 0 & (!!sym(rsp) %in% c(1, 2)) ~ !!sym(itm),
              !!sym(itm) == 97 ~ NA_real_
            )
          )
      }
    )
  } %>% 
  dplyr::mutate(blessed = blessedA + blessedB + blessedC + blessedD + blessedE + blessedF + blessedG + blessedH) %>% 
  dplyr::select(ADAMSSID, blessed)

## mortality at other ADAMS waves 
mortality <- ADAMS1TRK_R %>%
  dplyr::filter(!AMONTH == 97) %>%
  dplyr::select(ADAMSSID, BFRESULT, CVITSTAT) %>%
  dplyr::mutate(dead_b = as.numeric(BFRESULT == 7), dead_c = as.numeric(CVITSTAT == 5)) %>%
  dplyr::select(ADAMSSID, dead_b, dead_c)

#### Dementia diagnosis
## could potentially look at ADAMS Wave C for confirmation diagnosis in those alive

dementia <- ADAMS1AD_R %>%
  dplyr::select(ADAMSSID, ADFDX1) %>%
  dplyr::mutate(diagnosis = case_when(ADFDX1 %in% 1:2 ~ "Alzheimer's disease", ## probable and possible AD
                                      ADFDX1 %in% 3:4 ~ "Vascular dementia", ## probable and possible vascular dementia
                                      ADFDX1 %in% 5:19 ~ "Other dementia", ## dementia of undetermined etiology, alcoholic dementia, probable lewy body dementia, frontal lobe dementia, Parkinson's, normal pressure hydrocephalus, severe head trauma (with residual)
                                      ADFDX1 %in% 20:22 ~ "MCI", ## mild-ambiguous, cognitive impairment secondary to vascular disease, MCI 
                                      ADFDX1 %in% 28:30 ~ "MCI secondary to other conditions", ## stroke, other neurological, other medical 
                                      ADFDX1 == 31 ~ "Normal", 
                                      ADFDX1 %in% 23:27 ~ "Psychiatric and alcohol"),
                diagnosis_adams = case_when(diagnosis %in% c("Alzheimer's disease", "Vascular dementia", "Other dementia") ~ "Dementia",
                                            diagnosis %in% c("MCI", "MCI secondary to other conditions", "Psychiatric and alcohol") ~ "MCI",
                                            diagnosis %in% c("Normal") ~ "Normal"), 
                diagnosis_adjusted = case_when(diagnosis %in% c("Alzheimer's disease", "Vascular dementia", "Other dementia") ~ "Dementia",
                                               diagnosis %in% c("MCI", "MCI secondary to other conditions") ~ "MCI",
                                               diagnosis %in% c("Normal", "Psychiatric and alcohol") ~ "Normal"), 
                diagnosis_adams_2cat = case_when(diagnosis_adams == "Dementia" ~ "Dementia",
                                           diagnosis_adams %in% c("MCI", "Normal") ~ "No Dementia"),
                diagnosis_adams_2cat = factor(diagnosis_adams_2cat, levels = c("Dementia", "No Dementia")), 
                diagnosis_adjusted_2cat = case_when(diagnosis_adjusted == "Dementia" ~ "Dementia",
                                                    diagnosis_adjusted %in% c("MCI", "Normal") ~ "No Dementia"),
                diagnosis_adjusted_2cat = factor(diagnosis_adjusted_2cat, levels = c("Dementia", "No Dementia")),                                        
                diagnosis = factor(diagnosis, levels = c("Alzheimer's disease", "Vascular dementia", "Other dementia", "MCI", "MCI secondary to other conditions", "Psychiatric and alcohol", "Normal")), 
                diagnosis_adams = factor(diagnosis_adams, levels = c("Dementia", "MCI", "Normal")),
                diagnosis_adjusted = factor(diagnosis_adjusted, levels = c("Dementia", "MCI", "Normal")),
                stroke = as.numeric(ADFDX1 == 28)) %>%
  dplyr::left_join(ADAMS1BD_R %>% dplyr::select(ADAMSSID, BDFDX1), by = "ADAMSSID") %>%
  dplyr::mutate(diagnosis_b = case_when(BDFDX1 %in% 1:2 ~ "Alzheimer's disease", ## probable and possible AD
                                        BDFDX1 %in% 3:4 ~ "Vascular dementia", ## probable and possible vascular dementia
                                        BDFDX1 %in% 5:19 ~ "Other dementia", ## dementia of undetermined etiology, alcoholic dementia, probable lewy body dementia, frontal lobe dementia, Parkinson's, normal pressure hydrocephalus, severe head trauma (with residual)
                                        BDFDX1 %in% c(20:22, 33) ~ "MCI", ## mild-ambiguous, cognitive impairment secondary to vascular disease, MCI 
                                        BDFDX1 %in% 28:30 ~ "MCI secondary to other conditions", ## stroke, other neurological, other medical 
                                        BDFDX1 == 31 ~ "Normal", 
                                        BDFDX1 %in% 23:27 ~ "Psychiatric and alcohol"),
                diagnosis_adams_b = case_when(diagnosis_b %in% c("Alzheimer's disease", "Vascular dementia", "Other dementia") ~ "Dementia",
                                              diagnosis_b %in% c("MCI", "MCI secondary to other conditions", "Psychiatric and alcohol") ~ "MCI",
                                              diagnosis_b %in% c("Normal") ~ "Normal"), 
                diagnosis_adjusted_b = case_when(diagnosis_b %in% c("Alzheimer's disease", "Vascular dementia", "Other dementia") ~ "Dementia",
                                                 diagnosis_b %in% c("MCI", "MCI secondary to other conditions") ~ "MCI",
                                                 diagnosis_b %in% c("Normal", "Psychiatric and alcohol") ~ "Normal"), 
                diagnosis_2cat_b = case_when(diagnosis_adams_b == "Dementia" ~ "Dementia",
                                           diagnosis_adams_b %in% c("MCI", "Normal") ~ "No Dementia"),
                diagnosis_2cat_b = factor(diagnosis_2cat_b, levels = c("Dementia", "No Dementia")),                                         
                diagnosis_b = factor(diagnosis_b, levels = c("Alzheimer's disease", "Vascular dementia", "Other dementia", "MCI", "MCI secondary to other conditions", "Psychiatric and alcohol", "Normal")), 
                diagnosis_adams_b = factor(diagnosis_adams_b, levels = c("Dementia", "MCI", "Normal")),
                diagnosis_adjusted_b = factor(diagnosis_adjusted_b, levels = c("Dementia", "MCI", "Normal"))) %>%
  dplyr::left_join(ADAMS1CD_R %>% dplyr::select(ADAMSSID, CDFDX1), by = "ADAMSSID") %>%
  dplyr::mutate(diagnosis_c = case_when(CDFDX1 %in% 1:2 ~ "Alzheimer's disease", ## probable and possible AD
                                        CDFDX1 %in% 3:4 ~ "Vascular dementia", ## probable and possible vascular dementia
                                        CDFDX1 %in% 5:19 ~ "Other dementia", ## dementia of undetermined etiology, alcoholic dementia, probable lewy body dementia, frontal lobe dementia, Parkinson's, normal pressure hydrocephalus, severe head trauma (with residual)
                                        CDFDX1 %in% c(20:22, 33) ~ "MCI", ## mild-ambiguous, cognitive impairment secondary to vascular disease, MCI 
                                        CDFDX1 %in% 28:30 ~ "MCI secondary to other conditions", ## stroke, other neurological, other medical 
                                        CDFDX1 == 31 ~ "Normal", 
                                        CDFDX1 %in% 23:27 ~ "Psychiatric and alcohol"),
                diagnosis_adams_c = case_when(diagnosis_c %in% c("Alzheimer's disease", "Vascular dementia", "Other dementia") ~ "Dementia",
                                              diagnosis_c %in% c("MCI", "MCI secondary to other conditions", "Psychiatric and alcohol") ~ "MCI",
                                              diagnosis_c %in% c("Normal") ~ "Normal"), 
                diagnosis_adjusted_c = case_when(diagnosis_c %in% c("Alzheimer's disease", "Vascular dementia", "Other dementia") ~ "Dementia",
                                                 diagnosis_c %in% c("MCI", "MCI secondary to other conditions") ~ "MCI",
                                                 diagnosis_c %in% c("Normal", "Psychiatric and alcohol") ~ "Normal"), 
                diagnosis_2cat_c = case_when(diagnosis_adams_c == "Dementia" ~ "Dementia",
                                           diagnosis_adams_c %in% c("MCI", "Normal") ~ "No Dementia"),
                diagnosis_2cat_c = factor(diagnosis_2cat_c, levels = c("Dementia", "No Dementia")),                                         
                diagnosis_c = factor(diagnosis_c, levels = c("Alzheimer's disease", "Vascular dementia", "Other dementia", "MCI", "MCI secondary to other conditions", "Psychiatric and alcohol", "Normal")), 
                diagnosis_adams_c = factor(diagnosis_adams_c, levels = c("Dementia", "MCI", "Normal")),
                diagnosis_adjusted_c = factor(diagnosis_adjusted_c, levels = c("Dementia", "MCI", "Normal"))) %>%
  dplyr::select(ADAMSSID, diagnosis, diagnosis_adams, diagnosis_adjusted, diagnosis_adams_2cat, diagnosis_adjusted_2cat, stroke, 
                diagnosis_b, diagnosis_adams_b, diagnosis_adjusted_b, diagnosis_2cat_b,
                diagnosis_c, diagnosis_adams_c, diagnosis_adjusted_c, diagnosis_2cat_c) %>%
  dplyr::left_join(mortality, by = "ADAMSSID") %>%
  dplyr::mutate(diagnosis_adjusted2 = case_when(diagnosis == "MCI secondary to other conditions" & (diagnosis_2cat_b == "Dementia" | diagnosis_2cat_c == "Dementia") ~ "MCI", 
                                                diagnosis == "MCI secondary to other conditions" & (diagnosis_2cat_b == "No Dementia" | diagnosis_2cat_c == "No Dementia") ~ "Normal",
                                                diagnosis == "MCI secondary to other conditions" & (diagnosis_b == "MCI secondary to other conditions" & diagnosis_c == "MCI secondary to other conditions") ~ "Normal",
                                                TRUE ~ diagnosis_adjusted), 
                diagnosis_adjusted2 = factor(diagnosis_adjusted2, levels = c("Dementia", "MCI", "Normal")),
                diagnosis_adjusted2_2cat = case_when(diagnosis_adjusted2 == "Dementia" ~ "Dementia",
                                                     diagnosis_adjusted2 %in% c("MCI", "Normal") ~ "No Dementia"),
                diagnosis_adjusted2_2cat = factor(diagnosis_adjusted2_2cat, levels = c("Dementia", "No Dementia")))

## update with information from the future (for MCI due to other conditions)
# - if diagnosed with dementia at wave B -> MCI at wave A
# - if diagnosed with dementia at wave C -> MCI at wave A
# - if diagnosed normal at wave B -> normal at wave A 
# - if diagnosed normal at wave C -> normal at wave A 
# - if remain in the MCI secondary to other conditions category for all remaining waves, move to normal 

#### Demographics

demographics <- ADAMS1TRK_R %>%
  dplyr::filter(!AMONTH == 97) %>% ## exlcude those without Wave A assessment
  dplyr::select(ADAMSSID, age = AAGE, GENDER, ETHNIC, edyrs = EDYRS, degree = DEGREE, weight = AASAMPWT_F) %>%
  dplyr::mutate(age = na_if(age, 997), 
                age2 = age^2,
                edyrs = na_if(edyrs, 99), 
                female = as.numeric(GENDER == 2), 
                race = factor(case_when(ETHNIC == 1 ~ "White",
                                         ETHNIC == 2 ~ "Black",
                                         ETHNIC == 3 ~ "Hispanic"), 
                              levels = c("White", "Black", "Hispanic")), 
                degree = factor(case_when(degree %in% 0:1 ~ "Less than high school or GED",
                                          degree %in% 2:3 ~ "High school graduate", ## two-year degree in this category
                                          degree == 4 ~ "College graduate",
                                          degree %in% 5:6 ~ "Post-graduate"),
                                levels = c("Less than high school or GED", "High school graduate", "College graduate", "Post-graduate"))) %>%
  dplyr::select(ADAMSSID, age, age2, female, race, edyrs, degree, weight) 


  ## Self rated health 

sr_health2000 <- RAND2000 %>%
  dplyr::select(hhidpn, g1655) %>%
  dplyr::mutate(poormem2000 = as.numeric(g1655 == 3)) %>%
  dplyr::select(hhidpn, poormem2000)

sr_health2002 <- RAND2002 %>%
  dplyr::select(hhidpn, hd102) %>%
  dplyr::mutate(poormem2002 = as.numeric(hd102 == 3)) %>%
  dplyr::select(hhidpn, poormem2002)

sr_health <- ADAMS1TRK_R %>%
  dplyr::filter(!AMONTH == 97) %>%
  dplyr::select(HHID, PN, ADAMSSID, WAVESEL) %>%
  dplyr::mutate(hhidpn = as.numeric(paste0(HHID, PN))) %>%
  left_join(sr_health2000, by = "hhidpn") %>%
  left_join(sr_health2002, by = "hhidpn") %>%
  dplyr::mutate(poormem = ifelse(WAVESEL == 1, poormem2000, poormem2002), 
                poormem = coalesce(poormem, 0)) %>% ## assume no memory problems if missing
  dplyr::select(ADAMSSID, poormem)

## adls and iadls

disability_core <- GATEWAY %>%
  dplyr::select(hhidpn, r5adlfive, r6adlfive, r5iadlfour, r6iadlfour) %>%
  dplyr::rename(adl2000 = r5adlfive, adl2002 = r6adlfive, iadl2000 = r5iadlfour, iadl2002 = r6iadlfour)

disability <- ADAMS1TRK_R %>%
  dplyr::filter(!AMONTH == 97) %>%
  dplyr::select(HHID, PN, ADAMSSID, WAVESEL) %>%
  dplyr::mutate(hhidpn = as.numeric(paste0(HHID, PN))) %>%
  left_join(disability_core, by = "hhidpn") %>%
  dplyr::mutate(adl = ifelse(WAVESEL == 1, adl2000, adl2002), 
                iadl = ifelse(WAVESEL == 1, iadl2000, iadl2002)) %>%
  dplyr::mutate(iadl = ifelse(hhidpn == "203309010", iadl2000, iadl)) %>% ## deal with one person with missing iadl (due to don't do) and assign them to the wave earlier               
  dplyr::select(ADAMSSID, adl, iadl)

## creating of variables for 10/66 algorithm 
## adapted from https://github.com/chrissoria/1066_Algorithm/tree/main/ADAMS

binary_items <- c(
  "aPENCIL","aWATCH","aREPEAT","aTOWN","aCHIEF","aSTREET",
  "aADDRESS","aMONTH","aDAY","aYEAR","aSEASON","aPENTAG"
)

alg1066_cog <- ADAMS1AN_R %>% 
  select(
    ADAMSSID,
    aANIMALS  = ANAFTOT,
    aSTORY    = ANWM1TOT,
    aPENCIL   = ANMSE17,
    aREPEAT   = ANMSE7,
    aWATCH    = ANMSE16,
    aTOWN     = ANMSE8,
    aCHIEF    = ANPRES,
    aSTREET   = ANMSE10,
    aADDRESS  = ANMSE6,
    aMONTH    = ANMSE3,
    aDAY      = ANMSE5,
    aYEAR     = ANMSE1,
    aSEASON   = ANMSE2,
    aPENTAG   = ANMSE22, 
    aWORDIMM  = ANMSE11S, 
    aWORDDEL1 = ANMSE13, 
    aWORDDEL2 = ANMSE14,
    aWORDDEL3 = ANMSE15, 
    aPAPER1   = ANMSE20F, 
    aPAPER2   = ANMSE20L, 
    aPAPER3   = ANMSE20R
  ) %>% ## recode binary items/incorrect values
  mutate(across(
    all_of(binary_items),
    ~ case_when(
      .x == 97 ~ NA_real_,
      .x == 98 ~ 0,
      .x == 99 ~ NA_real_,
      TRUE ~ .x
    )
  )) %>%
  mutate(
    aWATCH  = if_else(aWATCH  == 2, 1, aWATCH),
    aPENCIL = if_else(aPENCIL == 2, 1, aPENCIL), 
    across(
      c(aANIMALS, aSTORY),
      ~ if_else(.x == 97, NA_real_, .x)
    )
  ) %>% ## sum iof binary items 
  mutate(
    count = rowSums(across(all_of(binary_items)), na.rm = FALSE),
    missing_count_cogscore = rowSums(is.na(across(all_of(binary_items))))
  ) %>% ## other cogscore components plus normalization 
  mutate(
    animtot = aANIMALS / 33, 
    across(c(aWORDDEL1, aWORDDEL2, aWORDDEL3, aPAPER1, aPAPER2, aPAPER3, aWORDIMM), ~ if_else(.x == 97, NA_real_, .x)),
    aWORDDEL = aWORDDEL1 + aWORDDEL2 + aWORDDEL3,
    aPAPER = aPAPER1 + aPAPER2 + aPAPER3, 
    wordtot1 = aWORDIMM / 3,
    wordtot2 = aWORDDEL / 3,
    papertot = aPAPER / 3,
    storytot = aSTORY / 37
  ) %>% ## get cogscore
  mutate(
    COGSCORE = 1.03125 * rowSums(
      cbind(count, animtot, wordtot1, wordtot2, papertot, storytot),
      na.rm = FALSE)
  ) %>%
  select(ADAMSSID, COGSCORE)

rels_vars <- c( ## for rel score + delayed recall (aRECALLcs)
  "aMEMORY","aFRDNAME","aFAMNAME","aLASTDAY","aORIENT",
  "aLOSTOUT","aLOSTIN","aCHORES","aMONEY","aREASON",
  "aWORDFIND", "aWORDWRG","aFEED","aDRESS","aTOILET","aHOBBYcs","aRECALLcs"
)

miss1_vars <- c(
  "aMEMORY","aFRDNAME","aFAMNAME","aWORDFIND","aWORDWRG",
  "aLASTDAY","aORIENT","aLOSTOUT","aLOSTIN",
  "aCHORES","aMONEY","aREASON","aHOBBYcs"
)

relsum_vars <- c(
  "aMEMORY","aFRDNAME","aFAMNAME","aWORDFIND","aWORDWRG",
  "aLASTDAY","aORIENT","aLOSTOUT","aLOSTIN","aCHORES",
  "aMONEY","aREASON","aFEED","aDRESS","aTOILET","aHOBBYcs"
)

alg1066_rel <- ADAMS1AD_R %>% 
  left_join(ADAMS1AN_R, by = "ADAMSSID") %>%
  left_join(ADAMS1AC_R, by = "ADAMSSID") %>%
  select(
    ADAMSSID,
    aMEMORY    = ADDRS1,
    aLASTDAY   = ADBL1G,
    aORIENT    = ADDRS2,
    aLOSTOUT   = ADBL1E,
    aLOSTIN    = ADBL1D,
    aCHORES    = ADBL1A,
    aMONEY     = ADBL1B,
    aREASON    = ADDRS3,
    aFEED      = ADBL2EA,
    aDRESS     = ADBL2DRE,
    aTOILET    = ADBL2TO,
    aHOBBYcs   = AC99,
    aRECALLcs  = ANDELCOR,     
    aFRDNAME   = ADDRS8,
    aFAMNAME   = ADDRS8,
    aWORDFIND  = ADDRS7, 
    aWORDWRG   = ADDRS7, 
    aDRESSDIS  = ADBL2DRR, 
    aTOILETDIS = ADBL2TOR, 
    aFEEDDIS   = ADBL2EAR, 
    aCHORESDIS = ADBL1AR 
  ) %>% ## missing data 
  mutate(across(
    all_of(rels_vars),
    ~ if_else(.x %in% c(97, 98, 99), NA_real_, as.numeric(.x))
  )) %>% ## recoding items 
  mutate(
    across(c(aWORDFIND, aWORDWRG), 
           ~ case_when(.x == 1 ~ 0,
                       .x == 2 ~ 0.5,
                       .x >= 3 ~ 1, 
                       TRUE ~ NA_real_)), 
    across(c(aMEMORY, aREASON), 
           ~ case_when(.x %in% c(1,2) ~ 0,
                       .x >= 3 ~ 1,
                       TRUE ~ NA_real_)),
    aHOBBYcs = case_when(
      aHOBBYcs == 4 ~ 0,
      aHOBBYcs %in% c(1, 2, 3) ~ 1,
      aHOBBYcs %in% c(7, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ), 
    across(c(aFRDNAME, aFAMNAME), 
           ~  case_when(.x %in% 1:3 ~ 0,
                        .x == 4 ~ 0.5,
                        .x %in% c(5, 6) ~ 1,
                        TRUE ~ NA_real_)), 
    aORIENT = case_when(
      aORIENT %in% c(1, 2) ~ 0,
      aORIENT %in% c(3, 4) ~ 0.5,
      aORIENT %in% c(5, 6) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>% ## do not count instances when unable due to physical disability 
  mutate(
    aDRESS  = if_else(aDRESSDIS  == 0, 0, aDRESS),
    aTOILET = if_else(aTOILETDIS == 0, 0, aTOILET),
    aFEED   = if_else(aFEEDDIS   == 0, 0, aFEED),
    aCHORES = if_else(aCHORESDIS == 0, 0, aCHORES)
  ) %>% ## create missing and summary scores 
  mutate( 
    MISS1 = rowSums(is.na(across(all_of(miss1_vars)))),
    MISS3 = rowSums(is.na(across(c(aFEED, aDRESS, aTOILET)))),
    MISSTOT = (MISS3 * 3) + MISS1,
    U = 23 / (23 - MISSTOT), 
    S = rowSums(across(all_of(relsum_vars), ~replace_na(.x, 0))),
    RELSCORE = U * S
  ) %>%
  select(ADAMSSID, RELSCORE, aRECALLcs)

# Stacking it up

join_list <- list(
  vdori1, vdmde1, vdmde2, vdmde3, vdmde4, vdmre1, vdmde6, vdexf2, vdexf8, vdexf9,
  vdasp1, vdasp2, vdasp3, vdlfl1, vdlfl2, vdlfl3, vdlfl4, vdlfl5, vdlfl7, vdlfl8,
  vdvis1, iqcode, blessed, dementia, demographics, sr_health, disability, alg1066_cog, alg1066_rel
)

tidied <- Reduce(function(x, y) left_join(x, y, by = "ADAMSSID"), join_list) %>%
  labelled::remove_labels()

readr::write_rds(tidied, here::here(rds_filepath, "010_tidy-data.rds"))
