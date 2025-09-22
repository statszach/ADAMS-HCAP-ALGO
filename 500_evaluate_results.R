rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

tidied <- readr::read_rds(here::here(rds_filepath, "010_tidy-data.rds"))
algo_data <- read_rds(paste0(rds_filepath, "hcapalgo.rds"))

## date for version control of figures 
date <- format(Sys.Date(), "%Y_%m_%d")

## relevel diagnoses so first level is normal 
diag_vars <- c("diagnosis_3cat", "diagnosis_2cat", "predicted_3cat", "predicted_2cat")
algo_data[, (diag_vars) := lapply(.SD, function(x) factor(x, levels = rev(levels(x)))), .SDcols = diag_vars]

# MAKE CONTINGENCY TABLES --------------------------------------------------

make_contingency_plot <- function(algorithm_data, truth, algo){

    ## get freq data
    data <- copy(algorithm_data)
    data[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
    table_dt <- data.table(xtabs(weight ~ get(algo) + get(truth), data = data))
    setnames(table_dt, c("algo", "truth", "freq"))

    ## get kappas 
    design <- survey::svydesign(ids = ~1, weights = ~weight, data = data)
    weighted_table <- survey::svytable(as.formula(paste0("~", algo, "+", truth)), design = design)
    kappas <- vcd::Kappa(weighted_table); kappas_confint <- confint(kappas)

    ## make plot 
    truth_levels <- data[, levels(get(truth))]
    table_dt[, truth := factor(truth, levels = truth_levels)]
    plot <- ggplot(table_dt, aes(x = truth, y = algo, fill = freq)) + 
        geom_tile(color = "black") + 
        geom_text(aes(label = sprintf("%.1f", freq))) + 
        scale_fill_gradient(low = "white", high = "#73498c") +
        scale_x_discrete(position = "top") +
        guides(fill = "none") +
        labs(y = "Algorithm diagnosis", x = "Clinical diagnosis") +
        theme_void() +
        theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold", margin = margin(b = 15)), 
              axis.title.y = element_text(angle = 90, margin = margin(r = 15)))

    ## make text grob (add weighted if more than 2 categories)
    text_toinclude <- paste0("Unweighted Kappa = ", sprintf("%.2f", kappas$Unweighted[1]), 
                                          " (95% CI: ", sprintf("%.2f", kappas_confint[1,1]), " to ", sprintf("%.2f", kappas_confint[1,2]), ")")
    if (data[, length(levels(get(truth)))] > 2){
        text_toinclude <- paste0(text_toinclude, "\nWeighted Kappa = ", sprintf("%.2f", kappas$Weighted[1]), 
                                          " (95% CI: ", sprintf("%.2f", kappas_confint[2,1]), " to ", sprintf("%.2f", kappas_confint[2,2]), ")")
    }                                          
    text_grob <- ggpubr::text_grob(text_toinclude, size = 12)

    combined_plot <- plot / patchwork::wrap_elements(text_grob) + plot_layout(heights = c(4, 1))
    return(combined_plot)
}

plot_3cat <- make_contingency_plot(algorithm_data = algo_data, truth = "diagnosis_3cat", algo = "predicted_3cat")
plot_2cat <- make_contingency_plot(algorithm_data = algo_data, truth = "diagnosis_2cat", algo = "predicted_2cat")

ggsave(paste0(images_filepath, "contingency_3cat_", date, ".pdf"), plot_3cat, width = 10, height = 5)
ggsave(paste0(images_filepath, "contingency_2cat_", date, ".pdf"), plot_2cat, width = 10, height = 5)

# LOOK AT OFF DIAGONALS -----------------------------------------------------------

## get proportion in each diagnosis category by disagreement type
disagreement_dt <- copy(algo_data[diagnosis_3cat != predicted_3cat, .(ADAMSSID, diagnosis_3cat, predicted_3cat, diagnosis, weight)])
disagreement_dt[, disagreement_direction := ifelse((diagnosis_3cat == "Normal" & predicted_3cat %in% c("MCI", "Dementia") | 
                                                   (diagnosis_3cat == "MCI" & predicted_3cat == "Dementia")), "Algorithm more severe", "Clinical diagnosis more severe")]
disagreement_dt[, combo := factor(paste0(diagnosis_3cat, " - ", predicted_3cat), 
                                         levels = c("Normal - Normal", "Normal - MCI", "Normal - Dementia", 
                                                    "MCI - Normal", "MCI - MCI", "MCI - Dementia", 
                                                    "Dementia - Normal", "Dementia - MCI", "Dementia - Dementia"))]  
disagreement_dt[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
disagreement_dt[, total_N_combo := sum(weight), by = combo] ## get denominators for each combo
disagreement_dt[, N_cat := paste0("N = ", .N), by = combo] ## get N for each combo
disagreement_diagnosis_dt <- disagreement_dt[, .(prop_diagnosis = sum(weight)/unique(total_N_combo), 
                                                 true_diagnosis = unique(diagnosis_3cat), 
                                                 N_label = unique(N_cat)), by = .(diagnosis, combo)]

## get proportion in each diagnois by true diagnosis category (for fairness of comparison)
agreement_dt <- copy(algo_data[diagnosis_3cat == predicted_3cat, .(ADAMSSID, diagnosis_3cat, predicted_3cat, diagnosis, weight)])
agreement_dt[, weight := scale(weight, center = FALSE, scale = sum(weight)/.N)] ## scale weights to sum to n
agreement_dt[, combo := factor(paste0(diagnosis_3cat, " - ", predicted_3cat), 
                                   levels = c("Normal - Normal", "Normal - MCI", "Normal - Dementia", 
                                                    "MCI - Normal", "MCI - MCI", "MCI - Dementia", 
                                                    "Dementia - Normal", "Dementia - MCI", "Dementia - Dementia"))]
agreement_dt[, total_N_agreement := sum(weight), by = combo] ## get denominators for each diagnosis type
agreement_dt[, N_cat := paste0("N = ", .N), by = combo] ## get N for each combo
agreement_diagnosis_dt <- agreement_dt[, .(prop_diagnosis = sum(weight)/unique(total_N_agreement), 
                                           true_diagnosis = unique(diagnosis_3cat),
                                           combo = unique(combo), N_label = unique(N_cat)), by = diagnosis]

## put together
full_diagnosis_dt <- rbind(disagreement_diagnosis_dt, agreement_diagnosis_dt)                                           
full_diagnosis_dt[, diagnosis := factor(diagnosis, levels = rev(c("Normal", "Other neurological", "Psychiatric", 
                                                              "MCI", "Possible AD", "Possible vascular dementia", 
                                                              "Probable AD", "Probable vascular dementia", "Other dementia")))]

## plot
disagreement_plot <- ggplot(full_diagnosis_dt, aes(x = combo, y = prop_diagnosis, fill = diagnosis)) + 
    geom_bar(stat = "identity", position = "stack") + 
    geom_text(aes(label = N_label, y = 1.02)) +
    facet_wrap(~true_diagnosis, nrow = 1, scales = "free_x") +
    scale_fill_manual(values = c("#2E598C", "#4b8ab1", "#9BD1F2",  "#f75e3b", "#f5825f", "#F7AF99", "#2b5627", "#59a852", "#B4CF66"), 
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
                                                            truth = "diagnosis_3cat", 
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
algo_data[, misclassified := as.numeric(diagnosis_3cat != predicted_3cat)]
algo_data[, underdiagnosed := as.numeric((diagnosis_3cat == "Dementia" & predicted_3cat %in% c("MCI", "Normal")) | 
                                         (diagnosis_3cat == "MCI" & predicted_3cat == "Normal"))]
algo_data[, overdiagnosed := as.numeric((diagnosis_3cat == "Normal" & predicted_3cat %in% c("MCI", "Dementia")) | 
                                        (diagnosis_3cat == "MCI" & predicted_3cat == "Dementia"))]

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
plot_breaks <- c(0.4,0.6,0.8,1,2,3,4); log_breaks <- log(plot_breaks)
rr_plot <- ggplot(rr_plot_dt, aes(x = cat_label, y = rr, ymin = rr_low, ymax = rr_high)) +
    geom_point(color = "#2d6a2d") +
    geom_errorbar(width = 0, color = "#2d6a2d") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~type, nrow = 1) +
    labs(x = "", y = "Relative risk") + 
    scale_x_discrete(breaks = rr_plot_dt[, levels(cat_label)], limits = discrete_lims) +
    scale_y_continuous(breaks = log_breaks, labels = plot_breaks, limits = c(log(0.3), log(6)), oob = scales::oob_keep) +
    coord_flip() +
    theme_bw()

ggsave(paste0(images_filepath, "rr_disagreement_", date, ".pdf"), rr_plot, width = 12, height = 6)

