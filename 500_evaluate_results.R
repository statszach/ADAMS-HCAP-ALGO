rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"
if (Sys.info()["sysname"] == "Windows") {
    code_filepath <- "C:/Users/emmanich/code/ADAMS-HCAP-ALGO/"
} else {
    code_filepath <- "/Users/emmanich/code/ADAMS-HCAP-ALGO/"
}
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

tidied <- readr::read_rds(here::here(rds_filepath, "010_tidy-data.rds"))
algo_data <- read_rds(paste0(rds_filepath, "hcapalgo.rds"))
results_1066 <- read_rds(paste0(rds_filepath, "results_1066.rds"))

## date for version control of figures 
date <- format(Sys.Date(), "%Y_%m_%d")

## relevel diagnoses so first level is normal 
diag_vars <- c("diagnosis_adams", "diagnosis_adjusted", "diagnosis_adjusted2", 
               "diagnosis_adams_2cat", "diagnosis_adjusted_2cat", "diagnosis_adjusted2_2cat",
               "predicted_3cat", "predicted_2cat", "predicted_2cat_s1", "predicted_3cat_s1",
               "predicted_2cat_s2", "predicted_3cat_s2", "predicted_2cat_s3", "predicted_3cat_s3")
algo_data[, (diag_vars) := lapply(.SD, function(x) factor(x, levels = rev(levels(x)))), .SDcols = diag_vars]

# MAKE GENERAL TABLES ------------------------------------------------------------------

fulldata <- as.data.table(Reduce(function(x, y) left_join(x, y, by = "ADAMSSID"), 
                                 list(tidied, algo_data[, .(ADAMSSID, diagnosis_true = diagnosis_adjusted_2cat, predicted_hcap = predicted_2cat)], 
                                      results_1066[, .(ADAMSSID, predicted_1066 = pred1066_dementia, prob1066 = pred1066_dementia_prob)])))

## relevel binary diagnosis so first level is No dementia in all cases
dem_cols <- c("diagnosis_true", "predicted_hcap", "predicted_1066")
fulldata[, (dem_cols) := lapply(.SD, function(x) relevel(x, ref = "Dementia")), .SDcols = dem_cols]

# Create binary indicators
fulldata[, dementia_true_num := as.numeric(diagnosis_true == "Dementia")]
fulldata[, dementia_hcap_num := as.numeric(predicted_hcap == "Dementia")]
fulldata[, dementia_1066_num := as.numeric(predicted_1066 == "Dementia")]

# survey design 
fulldata[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
fulldata_design <- svydesign(ids = ~1, weights = ~weight, data = fulldata)

# For diagnosis_true
totaln_dementia_true <- nrow(fulldata[!is.na(dementia_true_num)])
n_dementia_true <- fulldata[, sum(dementia_true_num, na.rm = TRUE)]
prevalence_true <- as.numeric(svymean(~dementia_true_num, fulldata_design))[1]

# For predicted_hcap
totaln_dementia_hcap <- nrow(fulldata[!is.na(dementia_hcap_num)])
n_dementia_hcap <- fulldata[, sum(dementia_hcap_num, na.rm = TRUE)]
prevalence_hcap <- as.numeric(svymean(~dementia_hcap_num, fulldata_design))[1]
# sens_hcap <- svymean(~dementia_hcap_num, subset(fulldata_design, dementia_true_num == 1)) these give same answers
# spec_hcap <- svymean(~I(dementia_hcap_num == 0), subset(fulldata_design, dementia_true_num == 0))
sens_hcap <- fulldata %>% yardstick::sens(truth = diagnosis_true, estimate = predicted_hcap, case_weights = weight) %>% pull(.estimate)
spec_hcap <- fulldata %>% yardstick::spec(truth = diagnosis_true, estimate = predicted_hcap, case_weights = weight) %>% pull(.estimate)
acc_hcap <- fulldata %>% yardstick::accuracy(truth = diagnosis_true, estimate = predicted_hcap, case_weights = weight) %>% pull(.estimate)
tpw_hcap <- fulldata[dementia_hcap_num == 1 & dementia_true_num == 1, sum(weight)]
tnw_hcap <- fulldata[dementia_hcap_num == 0 & dementia_true_num == 0, sum(weight)]
fpw_hcap <- fulldata[dementia_hcap_num == 1 & dementia_true_num == 0, sum(weight)]
fnw_hcap <- fulldata[dementia_hcap_num == 0 & dementia_true_num == 1, sum(weight)]

# For predicted_1066
totaln_dementia_1066 <- nrow(fulldata[!is.na(dementia_1066_num)])
n_dementia_1066 <- fulldata[, sum(dementia_1066_num, na.rm = TRUE)]
prevalence_1066 <- as.numeric(svymean(~dementia_1066_num, fulldata_design, na.rm = TRUE))[1]
# sens_1066 <- svymean(~dementia_1066_num, subset(fulldata_design, dementia_true_num == 1), na.rm = TRUE)
# spec_1066 <- svymean(~I(dementia_1066_num == 0), subset(fulldata_design, dementia_true_num == 0), na.rm = TRUE)
sens_1066 <- fulldata %>% yardstick::sens(truth = diagnosis_true, estimate = predicted_1066, case_weights = weight, na_rm = TRUE) %>% pull(.estimate)
spec_1066 <- fulldata %>% yardstick::spec(truth = diagnosis_true, estimate = predicted_1066, case_weights = weight, na_rm = TRUE) %>% pull(.estimate)
acc_1066 <- fulldata %>% yardstick::accuracy(truth = diagnosis_true, estimate = predicted_1066, case_weights = weight, na_rm = TRUE) %>% pull(.estimate)
auc_1066 <- fulldata %>% yardstick::roc_auc(truth = diagnosis_true, prob1066, case_weights = weight, na_rm = TRUE) %>% pull(.estimate)
tpw_1066 <- fulldata[dementia_1066_num == 1 & dementia_true_num == 1, sum(weight)]
tnw_1066 <- fulldata[dementia_1066_num == 0 & dementia_true_num == 0, sum(weight)]
fpw_1066 <- fulldata[dementia_1066_num == 1 & dementia_true_num == 0, sum(weight)]
fnw_1066 <- fulldata[dementia_1066_num == 0 & dementia_true_num == 1, sum(weight)]

# Create the table
summary_table <- data.table(
  category = c("diagnosis_true", "predicted_hcap", "predicted_1066"),
  n_dementia = c(n_dementia_true, n_dementia_hcap, n_dementia_1066),
  total_n = c(totaln_dementia_true, totaln_dementia_hcap, totaln_dementia_1066),
  weighted_prevalence = c(prevalence_true, prevalence_hcap, prevalence_1066),
  true_positives_weighted = c(NA, tpw_hcap, tpw_1066),
  true_negatives_weighted = c(NA, tnw_hcap, tnw_1066),
  false_positives_weighted = c(NA, fpw_hcap, fpw_1066),
  false_negatives_weighted = c(NA, fnw_hcap, fnw_1066),
  weighted_sensitivity = c(NA, sens_hcap, sens_1066),
  weighted_specificity = c(NA, spec_hcap, spec_1066),
  weighted_accuracy = c(NA, acc_hcap, acc_1066),
  auc = c(NA, NA, auc_1066)
)

## all columns to two decimal places where relevant
round_cols <- c("true_positives_weighted", "true_negatives_weighted", "false_positives_weighted", "false_negatives_weighted", "weighted_prevalence", 
                "weighted_sensitivity", "weighted_specificity", "weighted_accuracy", "auc")
summary_table[, (round_cols) := lapply(.SD, function(x) sprintf("%0.2f", x)), .SDcols = round_cols]

print(summary_table)

# MAKE CONTINGENCY TABLES --------------------------------------------------

## all combos for three-category classification
analysis_combos_3cat <- as.data.table(expand.grid(truths = c("diagnosis_adjusted", "diagnosis_adams", "diagnosis_adjusted2"), 
                                                  algorithms = c("predicted_3cat", "predicted_3cat_s1", "predicted_3cat_s2", "predicted_3cat_s3")))
analysis_combos_3cat[, names(analysis_combos_3cat) := lapply(.SD, as.character), .SDcols = names(analysis_combos_3cat)]

## repeat this for two-category versions 
analysis_combos_2cat <- copy(analysis_combos_3cat)
analysis_combos_2cat[, `:=` (truths = paste0(truths, "_2cat"), algorithms = gsub("3cat", "2cat", algorithms))]

## combine to full map 
version_map <- rbind(analysis_combos_3cat, analysis_combos_2cat)

make_contingency_plot <- function(algorithm_data = algo_data, row){

    truth <- version_map[, truths][row]; algo <- version_map[, algorithms][row]
    print(paste0(truth, " - ", algo))

    ## get freq data
    data <- copy(algorithm_data)
    data[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
    table_dt <- data.table(xtabs(weight ~ get(algo) + get(truth), data = data))
    setnames(table_dt, c("algo", "truth", "freq"))

    ## get kappas 
    design <- survey::svydesign(ids = ~1, weights = ~weight, data = data)
    weighted_table <- survey::svytable(as.formula(paste0("~", algo, "+", truth)), design = design)
    kappas <- vcd::Kappa(weighted_table); kappas_confint <- confint(kappas)

    ## get text 
    text_toinclude <- paste0("Unweighted Kappa = ", sprintf("%.2f", kappas$Unweighted[1]), 
                                          " (95% CI: ", sprintf("%.2f", kappas_confint[1,1]), " to ", sprintf("%.2f", kappas_confint[1,2]), ")")
    if (data[, length(levels(get(truth)))] > 2){
        text_toinclude <- paste0(text_toinclude, "\nWeighted Kappa = ", sprintf("%.2f", kappas$Weighted[1]), 
                                          " (95% CI: ", sprintf("%.2f", kappas_confint[2,1]), " to ", sprintf("%.2f", kappas_confint[2,2]), ")")
    }                            

    ## make plot 
    truth_levels <- data[, levels(get(truth))]
    table_dt[, truth := factor(truth, levels = truth_levels)]
    plot <- ggplot(table_dt, aes(x = truth, y = algo, fill = freq)) + 
        geom_tile(color = "black") + 
        geom_text(aes(label = sprintf("%.1f", freq))) + 
        scale_fill_gradient(low = "white", high = "#73498c") +
        scale_x_discrete(position = "top") +
        guides(fill = "none") +
        labs(y = "Algorithm diagnosis", x = "Clinical diagnosis", caption = text_toinclude) +
        theme_void() +
        theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8, face = "bold", margin = margin(b = 5)), 
              axis.title.y = element_text(angle = 90, margin = margin(r = 5)), plot.caption = element_text(hjust = 0.5, size = 8)) 

    return(plot)
}

all_plots <- lapply(1:nrow(version_map), function(x) make_contingency_plot(row = x))

## create plot grids 

## for diagnosis based on adjusted ADAMS, ADAMS, adjusted ADAMS (2) - using future for CIND with medical conditions

## predictions for normative sample with adjusted ADAMS
grid1 <- wrap_plots(all_plots[c(1:3, 13:15)]) + plot_layout(nrow = 2)

## predictions for normative sample with ADAMS 
grid2 <- wrap_plots(all_plots[c(4:6, 16:18)]) + plot_layout(nrow = 2)

## predictions for normative sample with adjusted ADAMS v2 
grid3 <- wrap_plots(all_plots[c(7:9, 19:21)]) + plot_layout(nrow = 2)

## predictions for normative sample with adjusted ADAMS and additional exclusion of those with IADL limitations 
grid4 <- wrap_plots(all_plots[c(10:12, 22:24)]) + plot_layout(nrow = 2)

## save all options in same file 
pdf(paste0(images_filepath, "all_contingency_tables_", date, ".pdf"), width = 10, height = 5)
grid1; grid2; grid3; grid4
dev.off()

## based on this we are going to take adjusted ADAMS with the adjusted ADAMS normative sample as our main analysis

# LOOK AT OFF DIAGONALS -----------------------------------------------------------

## get proportion in each diagnosis category by disagreement type
disagreement_dt <- copy(algo_data[diagnosis_adjusted != predicted_3cat, .(ADAMSSID, diagnosis_adjusted, predicted_3cat, diagnosis, weight)])
disagreement_dt[, disagreement_direction := ifelse((diagnosis_adjusted == "Normal" & predicted_3cat %in% c("MCI", "Dementia") | 
                                                   (diagnosis_adjusted == "MCI" & predicted_3cat == "Dementia")), "Algorithm more severe", "Clinical diagnosis more severe")]
disagreement_dt[, combo := factor(paste0(diagnosis_adjusted, " - ", predicted_3cat), 
                                         levels = c("Normal - Normal", "Normal - MCI", "Normal - Dementia", 
                                                    "MCI - Normal", "MCI - MCI", "MCI - Dementia", 
                                                    "Dementia - Normal", "Dementia - MCI", "Dementia - Dementia"))]  
disagreement_dt[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
disagreement_dt[, total_N_combo := sum(weight), by = combo] ## get denominators for each combo
disagreement_dt[, N_cat := paste0("N = ", .N), by = combo] ## get N for each combo
disagreement_diagnosis_dt <- disagreement_dt[, .(prop_diagnosis = sum(weight)/unique(total_N_combo), 
                                                 true_diagnosis = unique(diagnosis_adjusted), 
                                                 N_label = unique(N_cat)), by = .(diagnosis, combo)]

## get proportion in each diagnois by true diagnosis category (for fairness of comparison)
agreement_dt <- copy(algo_data[diagnosis_adjusted == predicted_3cat, .(ADAMSSID, diagnosis_adjusted, predicted_3cat, diagnosis, weight)])
agreement_dt[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
agreement_dt[, combo := factor(paste0(diagnosis_adjusted, " - ", predicted_3cat), 
                                   levels = c("Normal - Normal", "Normal - MCI", "Normal - Dementia", 
                                                    "MCI - Normal", "MCI - MCI", "MCI - Dementia", 
                                                    "Dementia - Normal", "Dementia - MCI", "Dementia - Dementia"))]
agreement_dt[, total_N_agreement := sum(weight), by = combo] ## get denominators for each diagnosis type
agreement_dt[, N_cat := paste0("N = ", .N), by = combo] ## get N for each combo
agreement_diagnosis_dt <- agreement_dt[, .(prop_diagnosis = sum(weight)/unique(total_N_agreement), 
                                           true_diagnosis = unique(diagnosis_adjusted),
                                           combo = unique(combo), N_label = unique(N_cat)), by = diagnosis]

## put together
full_diagnosis_dt <- rbind(disagreement_diagnosis_dt, agreement_diagnosis_dt)                                           
full_diagnosis_dt[, diagnosis := factor(diagnosis, levels = rev(c("Normal", "Psychiatric and alcohol", 
                                                                  "MCI", "MCI secondary to other conditions", 
                                                                  "Alzheimer's disease", "Vascular dementia", "Other dementia")))]

## plot
disagreement_plot <- ggplot(full_diagnosis_dt, aes(x = combo, y = prop_diagnosis, fill = diagnosis)) + 
    geom_bar(stat = "identity", position = "stack") + 
    geom_text(aes(label = N_label, y = 1.02)) +
    facet_wrap(~true_diagnosis, nrow = 1, scales = "free_x") +
    scale_fill_manual(values = c("#2E598C", "#9BD1F2",  "#f75e3b", "#F7AF99", "#2b5627", "#59a852", "#B4CF66"), 
    breaks = rev(full_diagnosis_dt[, levels(diagnosis)])) +
    scale_x_discrete(labels = scales::label_wrap(12)) +
    labs(x = "Classification \n(Clinical - Algorithm)", y = "Proportion with diagnosis", fill = "Clinical diagnosis") +
    theme_bw()

ggsave(paste0(images_filepath, "disagreement_diagnoses_", date, ".pdf"), disagreement_plot, width = 12, height = 6)

# KAPPA BY OTHER CHARACTERISTICS --------------------------------------------------

## make factors with appropriate levels (+ race, already coded fine)
algo_data[, age_group := factor(cut(age, breaks = c(69, 79, 89, Inf), labels = c("70-79", "80-89", "90+")))]
algo_data[, gender := factor(ifelse(female == 0, "Men", "Women"))]
algo_data[, educ := factor(fcase(degree == "Less than high school or GED", "Less than high school or GED", 
                                 degree == "High school graduate", "High school graduate",
                                 degree %in% c("College graduate", "Post-graduate"), "College and up"), 
                           levels = c("Less than high school or GED", "High school graduate", "College and up"))]

get_kappa_bycat <- function(algorithm_data, truth, algo, cat){

    ## get data 
    data <- copy(algorithm_data)
    data[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N), by = cat] ## scale weights to sum to n
    
    ## get kappas 
    designs <- lapply(data[, levels(get(cat))], function(x) survey::svydesign(ids = ~1, weights = ~weight, data = data[get(cat) == x]))
    weighted_tables <- lapply(designs, function(x) survey::svytable(as.formula(paste0("~", algo, "+", truth)), design = x))
    kappas <- lapply(weighted_tables, function(x) vcd::Kappa(x))
    kappas_confint <- lapply(kappas, confint)

    ## create results table
    kappa_dt <- rbindlist(lapply(1:data[, length(levels(get(cat)))], function(x) 
                                 data.table(var = cat,
                                            category = data[, levels(get(cat))][x],
                                            type = c("Unweighted", "Weighted"),
                                            mean = c(kappas[[x]]$Unweighted[1], kappas[[x]]$Weighted[1]),
                                            lower = c(kappas_confint[[x]][1,1], kappas_confint[[x]][2,1]),
                                            upper = c(kappas_confint[[x]][1,2], kappas_confint[[x]][2,2]))))

    if (data[, length(levels(get(truth)))] == 2){
        kappa_dt <- kappa_dt[type == "Unweighted"] ## only keep unweighted if 2 categories
    }                 

    return(kappa_dt)
}

## get kappas for 3 category version 
kappas_3cat <- rbindlist(lapply(c("age_group", "gender", "educ", "race"), 
                                function(x) get_kappa_bycat(algorithm_data = algo_data, 
                                                            truth = "diagnosis_adjusted", 
                                                            algo = "predicted_3cat", 
                                                            cat = x)))

## format table 
kappas_3cat[, variable := factor(fcase(var == "age_group", "Age group",
                                  var == "gender", "Gender",
                                  var == "educ", "Education",
                                  var == "race", "Race"), 
                            levels = c("Age group", "Gender", "Education", "Race"))]
kappas_3cat[, category := factor(category, levels = c("70-79", "80-89", "90+", "Men", "Women", 
                                                      "Less than high school or GED", "High school graduate", "College and up",
                                                      "White", "Black", "Hispanic"))]
kappas_3cat[, `:=` (value = paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", lower), " to ", sprintf("%.2f", upper), ")"))]
kappas_3cat <- dcast(kappas_3cat, variable + category ~ type, value.var = "value")
kappas_3cat <- kappas_3cat[order(variable, category)]

openxlsx::write.xlsx(kappas_3cat, paste0(images_filepath, "kappas_bychar_3cat_", date, ".xlsx"))

# RISK RATIOS FOR UNDER/OVER DIAGNOSIS -------------------------------------------

## get indicator variable 
algo_data[, misclassified := as.numeric(diagnosis_adjusted != predicted_3cat)]
algo_data[, underdiagnosed := as.numeric((diagnosis_adjusted == "Dementia" & predicted_3cat %in% c("MCI", "Normal")) | 
                                         (diagnosis_adjusted == "MCI" & predicted_3cat == "Normal"))]
algo_data[, overdiagnosed := as.numeric((diagnosis_adjusted == "Normal" & predicted_3cat %in% c("MCI", "Dementia")) | 
                                        (diagnosis_adjusted == "MCI" & predicted_3cat == "Dementia"))]

## set normal to be the reference for diagnosis
algo_data[, diagnosis := relevel(diagnosis, ref = "Normal")]

## set up predictor variables 
predictors <- c("age_group", "gender", "educ", "race")

## survey design
rr_design <- survey::svydesign(ids = ~1, weights = ~weight, data = algo_data)

## function to get risk ratios
get_riskratios <- function(predictor){

    print(predictor)

    ## misclassification risk ratios 
    m_model <- survey::svyglm(as.formula(paste0("misclassified ~ ", predictor)), design = rr_design, family = poisson())
    m_params <- parameters::parameters(m_model)
    m_select <- grepl(predictor, m_params$Parameter)
    m_params_dt <- data.table(cat = m_params$Parameter[m_select], rr = m_params$Coefficient[m_select], 
                       rr_low = m_params$CI_low[m_select], rr_high = m_params$CI_high[m_select])
    m_dt <- rbind(data.table(cat = paste0(predictor, algo_data[, levels(get(predictor))][1]), rr = 0, rr_low = NA, rr_high = NA), 
                             m_params_dt)
    m_dt[, `:=` (pred = predictor, type = "Total misclassification")]                             

    ## underdiagnosis risk ratios 
    u_model <- survey::svyglm(as.formula(paste0("underdiagnosed ~ ", predictor)), design = rr_design, family = poisson())
    u_params <- parameters::parameters(u_model)
    u_select <- grepl(predictor, u_params$Parameter)
    u_params_dt <- data.table(cat = u_params$Parameter[u_select], rr = u_params$Coefficient[u_select], 
                       rr_low = u_params$CI_low[u_select], rr_high = u_params$CI_high[u_select])
    u_dt <- rbind(data.table(cat = paste0(predictor, algo_data[, levels(get(predictor))][1]), rr = 0, rr_low = NA, rr_high = NA), 
                             u_params_dt)
    u_dt[, `:=` (pred = predictor, type = "Algorithm < Clinical")]                          

    ## overdiagnosis risk ratios 
    o_model <- survey::svyglm(as.formula(paste0("overdiagnosed ~ ", predictor)), design = rr_design, family = poisson())
    o_params <- parameters::parameters(o_model)
    o_select <- grepl(predictor, o_params$Parameter)
    o_params_dt <- data.table(cat = o_params$Parameter[o_select], rr = o_params$Coefficient[o_select], 
                       rr_low = o_params$CI_low[o_select], rr_high = o_params$CI_high[o_select])
    o_dt <- rbind(data.table(cat = paste0(predictor, algo_data[, levels(get(predictor))][1]), rr = 0, rr_low = NA, rr_high = NA), 
                             o_params_dt)
    o_dt[, `:=` (pred = predictor, type = "Algorithm > Clinical")]                           

    ## combine together 
    results <- rbindlist(list(m_dt, u_dt, o_dt))

    return(results)
}

rr_results <- rbindlist(lapply(predictors, get_riskratios))

## create necessary factors with correct order for plotting
rr_plot_dt <- copy(rr_results)
rr_plot_dt[, cat_label := cat][, cat_label := gsub("age_group", "Age Group: ", cat_label)][, cat_label := gsub("gender", "Gender: ", cat_label)] 
rr_plot_dt[, cat_label := gsub("educ", "Education: ", cat_label)][, cat_label := gsub("race", "Race/ethnicity: ", cat_label)]
rr_plot_dt[, cat_label := factor(cat_label, levels = rr_plot_dt[, rev(unique(cat_label))])]
rr_plot_dt[, type := factor(type, levels = c("Total misclassification", "Algorithm < Clinical", "Algorithm > Clinical"))]

## create spacers for discrete axis 
cats <- unique(str_extract(rr_plot_dt[, levels(cat_label)], ".*?(?=:)"))
discrete_lims <- unlist(lapply(1:length(cats), function(x) c(str_subset(rr_plot_dt[, levels(cat_label)], cats[x]), "skip")))
discrete_lims <- discrete_lims[-length(discrete_lims)] ## remove the last skip

## plot results
plot_breaks <- c(0.2,0.4,0.6,0.8,1,2,3,4); log_breaks <- log(plot_breaks)
y_limits <- c(log(0.2), log(4.2))
rr_plot_dt[, lower_cap := pmax(rr_low, y_limits[1])]
rr_plot_dt[, upper_cap := pmin(rr_high, y_limits[2])]

rr_plot <- ggplot(rr_plot_dt, aes(x = cat_label, y = rr, ymin = rr_low, ymax = rr_high)) +
    geom_point(color = "#2d6a2d") +
    geom_errorbar(width = 0, color = "#2d6a2d") + 
    geom_segment(data = rr_plot_dt[rr_low < y_limits[1]], aes(x = cat_label, xend = cat_label, y = upper_cap, yend = lower_cap),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"), show.legend = FALSE, color = "#2d6a2d") +
    geom_segment(data = rr_plot_dt[rr_high > y_limits[2]], aes(x = cat_label, xend = cat_label, y = lower_cap, yend = upper_cap),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"), show.legend = FALSE, color = "#2d6a2d") +      
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~type, nrow = 1) +
    labs(x = "", y = "Relative risk") + 
    scale_x_discrete(breaks = rr_plot_dt[, levels(cat_label)], limits = discrete_lims) +
    scale_y_continuous(breaks = log_breaks, labels = plot_breaks, limits = y_limits, oob = scales::oob_keep, expand = expansion(0)) +
    coord_flip() +
    theme_bw()

ggsave(paste0(images_filepath, "rr_disagreement_", date, ".pdf"), rr_plot, width = 12, height = 6)

