rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

load(here::here(rds_filepath, "010_tidy-data.Rdata"))
algo_data <- read_rds(paste0(rds_filepath, "hcapalgo.rds"))

## relevel diagnoses so first level is normal 
diag_vars <- c("diagnosis_3cat", "diagnosis_2cat", "predicted_3cat", "predicted_2cat")
algo_data[, (diag_vars) := lapply(.SD, function(x) factor(x, levels = rev(levels(x)))), .SDcols = diag_vars]

# MAKE CONTINGENCY TABLES --------------------------------------------------

data = copy(algo_data); truth = "diagnosis_2cat"; algo = "predicted_2cat"

make_contingency_plot <- function(data, truth, algo){

    ## get freq data
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
        theme_minimal()

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

plot_3cat <- make_contingency_plot(data = algo_data, truth = "diagnosis_3cat", algo = "predicted_3cat")
plot_2cat <- make_contingency_plot(data = algo_data, truth = "diagnosis_2cat", algo = "predicted_2cat")

# LOOK AT OFF DIAGONALS -----------------------------------------------------------