rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

load(here::here(rds_filepath, "010_tidy-data.Rdata"))
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
                                         levels = c("Normal - Normal", "MCI - MCI", "Dementia - Dementia", 
                                                    "Normal - Dementia", "Normal - MCI", "MCI - Dementia", "Dementia - MCI", "MCI - Normal", "Dementia - Normal"))]  
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
                                   levels = c("Normal - Normal", "MCI - MCI", "Dementia - Dementia", 
                                              "Normal - Dementia", "Normal - MCI", "MCI - Dementia", "Dementia - MCI", "MCI - Normal", "Dementia - Normal"))]
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
    scale_x_discrete(labels = label_wrap(12)) +
    labs(x = "Classification \n(True - Algorithm)", y = "Proportion with diagnosis", fill = "Clinical diagnosis") +
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

write.xlsx(kappas_3cat, paste0(images_filepath, "kappas_bychar_3cat_", date, ".xlsx"))
