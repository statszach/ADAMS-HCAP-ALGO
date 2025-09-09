rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
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

blessed <- ADAMS1AD_R %>% 
  

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










%>%
  dplyr::mutate(blessed1 = dplyr::case_when(ADBL1A == 0 ~ 0,
                                            ADBL1A > 0 & ADBL1AR == 0 ~ 0,
                                            ADBL1A > 0 & (ADBL1AR == 1 | ADBL1AR == 2) ~ ADBL1A,
                                            ADBL1A == 97 ~ NA_real_))
  
  
  dplyr::mutate(across(contains("ADBL"), ~ replace(.x, .x %in% c(97), NA))) %>%
  dplyr::mutate(blessed_sum = rowSums(across(ADBL1A:ADBL1H), na.rm = TRUE)) %>% 
  dplyr::select(ADAMSSID, blessed_sum)

## Check how to score blessed

# checkblessedhcap <- hc16hp_i %>% 
#   dplyr::select(INF1BL1_1, INF1BL1_2, INF1BL1_3, INF1BL1_4, INF1BL1_5,
#                 INF1BL1_6, INF1BL1_7, INF1BL1_8) %>% 
#   dplyr::mutate(across(contains("INF1"), ~ replace(.x, .x %in% c(8, 9), NA))) %>%
#   dplyr::mutate(blessed_sum = rowSums(across(INF1BL1_1:INF1BL1_8), na.rm = TRUE)/3)
  
# psych::describe(checkblessedhcap$blessed_sum)
# psych::describe(hc16hp_i$INF1BL1_SCORE)

#### Dementia diagnosis
## could potentially look at ADAMS Wave C for confirmation diagnosis in those alive

dementia <- ADAMS1AD_R %>%
  dplyr::select(ADAMSSID, ADFDX1) %>%
  dplyr::mutate(diagnosis = case_when(ADFDX1 == 1 ~ "Probable AD", 
                                      ADFDX1 == 2 ~ "Possible AD", 
                                      ADFDX1 == 3 ~ "Probable vascular dementia", 
                                      ADFDX1 == 4 ~ "Possible vascular dementia", 
                                      ADFDX1 %in% c(10,13,15,18) ~ "Other dementia", ## dementia of undetermined etiology, frontal lobe dementia, alcoholic dementia, probable lewy body dementia
                                      ADFDX1 %in% c(5,8,14,28,29) ~ "Other neurological", ## Parkinson's, normal pressure hydrocephalus, severe head trauma, stroke, other neurological conditions
                                      ADFDX1 %in% c(20,21,22) ~ "MCI", ## mild-ambiguous, cognitive impairment secondary to vascular disease, MCI
                                      ADFDX1 %in% c(23:25) ~ "Psychiatric", ## depression, psychiatric disorder, mental retardation
                                      ADFDX1 %in% c(26,27,30,31) ~ "Normal"), ## alcohol abuse (past), alcohol abuse (current), other medical conditions, normal/non-case
                diagnosis_3cat = case_when(diagnosis %in% c("Probable AD", "Probable vascular dementia", "Other dementia") ~ "Dementia",
                                           diagnosis %in% c("Possible AD", "Possible vascular dementia", "MCI") ~ "MCI",
                                           diagnosis %in% c("Normal", "Other neurological", "Psychiatric") ~ "Normal"), 
                diagnosis_2cat = case_when(diagnosis_3cat == "Dementia" ~ "Dementia",
                                           diagnosis_3cat %in% c("MCI", "Normal") ~ "No Dementia"),
                diagnosis_2cat = factor(diagnosis_2cat, levels = c("Dementia", "No Dementia")),                                         
                diagnosis = factor(diagnosis, levels = c("Probable AD", "Possible AD", "Probable vascular dementia", "Possible vascular dementia", "Other dementia", "Other neurological", "MCI", "Psychiatric", "Normal")), 
                diagnosis_3cat = factor(diagnosis_3cat, levels = c("Dementia", "MCI", "Normal"))) %>%
  dplyr::select(ADAMSSID, diagnosis, diagnosis_3cat, diagnosis_2cat)


#### Demographics

demographics <- ADAMS1TRK_R %>%
  dplyr::filter(!AMONTH == 97) %>% ## exlcude those without Wave A assessment
  dplyr::select(ADAMSSID, age = AAGE, GENDER, ETHNIC, edyrs = EDYRS, degree = DEGREE, weight = AASAMPWT_F) %>%
  dplyr::mutate(age = na_if(age, 997), 
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
  dplyr::select(ADAMSSID, age, female, race, edyrs, degree, weight) 


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

# Stacking it up

join_list <- list(
  vdori1, vdmde1, vdmde2, vdmde3, vdmde4, vdmre1, vdmde6, vdexf2, vdexf8, vdexf9,
  vdasp1, vdasp2, vdasp3, vdlfl1, vdlfl2, vdlfl3, vdlfl4, vdlfl5, vdlfl7, vdlfl8,
  vdvis1, iqcode, blessed, dementia, demographics, sr_health
)

tidied <- Reduce(function(x, y) left_join(x, y, by = "ADAMSSID"), join_list) %>%
  labelled::remove_labels()

save.image(here::here(rds_filepath, "010_tidy-data.Rdata"))
