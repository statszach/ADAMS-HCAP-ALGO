rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

tidied <- readr::read_rds(here::here(rds_filepath, "010_tidy-data.rds"))
fscores <- readr::read_rds(paste0(rds_filepath, "fscores.rds"))

domain_scores <- c("ORI", "MEM", "EXF", "LFL", "VIS")

# COMBINE DATA -----------------------------------------------------------

data <- as.data.table(merge(tidied, fscores, by = "ADAMSSID"))

## using Rich's knots for spline, could change to looking at ADAMS data separately 
adjust_vars <- c("splines::ns(age, knots = c(78, 86, 94))", "female", "edyrs", "race")

# FORMAT DATA ------------------------------------------------------------

## set other domain names 
setnames(data, c("vdori1", "vdvis1"), c("ORI", "VIS"))

## impute if missing based on other domains 
data <- simputation::impute_pmm(data, EXF ~ MEM + LFL + ORI)
data <- simputation::impute_pmm(data, VIS ~ MEM + EXF + LFL + ORI)

## need imputation model for iqcode - just using this simple one for now
data <- simputation::impute_pmm(data, blessed ~ age + adl + iadl)
data <- simputation::impute_pmm(data, iqcode_mean ~ age + blessed + adl + iadl)

## define severe and moderate function loss according to cut points from Manly-Jones
data[, severe_function := as.numeric(iqcode_mean >= 3.4 | blessed >= 2)]
data[, moderate_function := as.numeric(iqcode_mean > 3 | blessed > 0)]

# CLASSIFICATION -----------------------------------------------------------

classify <- function(all_data = data, normative_data){
    
    dt <- copy(all_data); normative_dt <- copy(normative_data)

    ## blom transformed versions in normative sample
    blom_transform <- function(x){
        ranks <- rank(x, na.last = "keep")
        return(qnorm((ranks-0.375)/(length(x)+0.25)))
    }
    normative_dt[, paste0(domain_scores, "_b") := lapply(.SD, blom_transform), .SDcols = domain_scores]

    ## get blom transformation in full sample via parametric model
    blom_design <- survey::svydesign(ids = ~1, weights = ~weight, data = normative_dt)
    blom_models <- lapply(domain_scores, function(score){
        quantiles <- survey::svyquantile(as.formula(paste0("~", score)), design = blom_design, quantiles = c(0.05, 0.35, 0.65, 0.95))
        knot_locations <- unique(quantiles[[1]][,1])
        knot_locations <- knot_locations[!knot_locations %in% c(normative_dt[, min(get(score))], normative_dt[, max(get(score))])] ## knots can't be the score min or max
        formula <- paste0(score, "_b ~ splines::ns(", score, ", knots = c(", paste(knot_locations, collapse = ", "), "))")
        return(survey::svyglm(formula, design = blom_design))
    })

    ## get predicted blom in the full sample 
    setDT(dt) ## tbh I'm not totally sure why this is needed, but it has to do with shallow copies, and it gets rid of a warning for below
    dt[, paste0(domain_scores, "_b_pred") := 
        lapply(1:length(domain_scores), function(x) predict(blom_models[[x]], newdata = dt))]

    ## add predicted blom scores to normative dataset (Q: use true blom transformed or prediction, currently prediction)
    setDT(normative_dt) ## tbh I'm not totally sure why this is needed, but it has to do with shallow copies, and it gets rid of a warning for below
    normative_dt[, paste0(domain_scores, "_b_pred") := 
                lapply(1:length(domain_scores), function(x) predict(blom_models[[x]], newdata = normative_dt))]

    ## get SD of blom in normative sample 
    blom_sds <- as.numeric(normative_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = paste0(domain_scores, "_b_pred")])

    ## add all two-way interactions to adjustment vars
    interaction_terms <- combn(adjust_vars, 2, FUN = function(x) paste0(x[1], ":", x[2]))

    ## normative models
    normative_design <- survey::svydesign(ids = ~1, weights = ~weight, data = normative_dt)
    norms_models <- lapply(domain_scores, function(score){
        formula <- paste0(score, "_b_pred ~ ", paste(c(adjust_vars, interaction_terms), collapse = " + "))
        return(survey::svyglm(formula, design = normative_design))
    })

    ## get model r2s 
    norms_r2 <- sapply(norms_models, function(x) 1 - (x$deviance / x$null.deviance))

    ## expected cognition based on norms
    dt[, paste0(domain_scores, "_b_norms") := 
                lapply(1:length(domain_scores), function(x) predict(norms_models[[x]], newdata = dt))]

    ## calculate T-scores
    dt[, paste0(domain_scores, "_T_score") := 
        lapply(1:length(domain_scores), 
        function(x) (50+
                        10*(get(paste0(domain_scores[x], "_b_pred"))-get(paste0(domain_scores[x], "_b_norms")))/
                        (blom_sds[x]*sqrt(1 - norms_r2[x]))))]

    ## check t-score mean and sd (should be 50, 10) in ADAMS sample 
    check_tscore <- dt[ADAMSSID %in% normative_dt[, ADAMSSID], 
                            lapply(.SD, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))), 
                            .SDcols = paste0(domain_scores, "_T_score")]
    if (any(sapply(check_tscore[1,], function(x) abs(x-50) >= 4)) | any(sapply(check_tscore[2,], function(x) abs(x-10) >= 2))) {
        warning("T-score mean and/or sd not equal to 50 and/or 10 in Reference Sample"); print(check_tscore)
    }

    ## define impairment as lower than 36 for each domain 
    dt[, paste0(domain_scores, "_impairment") := 
        lapply(1:length(domain_scores), function(x) ifelse(get(paste0(domain_scores[x], "_T_score")) < 36, 1, 0))]

    ## override orientation impairment by using orientation less than 8 as the criteria
    dt[, ORI_impairment := ifelse(ORI <= 8, 1, 0)]

    ## number of impaired domains 
    dt[, num_impaired_domains := rowSums(.SD), .SDcols = paste0(domain_scores, "_impairment")]

    ## make sure that there is no missingness in any important predictors
    if (nrow(dt[is.na(num_impaired_domains) | is.na(severe_function) | is.na(moderate_function)]) > 0) stop("There is missingness in classification, check contributing variables")

    dt[, dementia := as.numeric(num_impaired_domains >= 2 & severe_function == 1)]
    dt[, mci := as.numeric((num_impaired_domains >= 2 & severe_function == 0) |
                            (num_impaired_domains == 1 & moderate_function == 1) | 
                            (num_impaired_domains == 1 & moderate_function == 0 & poormem == 1))]

    ## predicted three-cat 
    dt[, predicted_3cat := factor(fcase(dementia == 1, "Dementia", 
                                        mci == 1, "MCI", 
                                        (!dementia == 1 & !mci == 1), "Normal"), 
                                    levels = c("Dementia", "MCI", "Normal"))]
    dt[, predicted_2cat := factor(fcase(predicted_3cat == "Dementia", "Dementia", 
                                        predicted_3cat %in% c("MCI", "Normal"), "No Dementia"))]

    return(dt)
}

class_dt1 <- classify(all_data = data, normative_data = data[diagnosis_3cat == "Normal"])
class_dt2 <- classify(all_data = data, normative_data = data[diagnosis_3cat == "Normal" & stroke == 0])

# PRINT RESULTS -----------------------------------------------------------

print_results <- function(data){
    ## numbers for figure
    message(paste0("Total N: ", nrow(data)))
    message(paste0("Impaired in 2+ domains: ", nrow(data[num_impaired_domains >= 2])))
    message(paste0("Impaired in 2+ domains + severe functional impairment: ", nrow(data[dementia == 1])))
    message(paste0("Impaired in 2+ domains + no severe functional impairment: ", nrow(data[num_impaired_domains >= 2 & severe_function == 0])))
    message(paste0("Not impaired in 2+ domains: ", nrow(data[num_impaired_domains < 2])))
    message(paste0("Impaired in 1 domain: ", nrow(data[num_impaired_domains == 1])))
    message(paste0("No cognitive impairment: ", nrow(data[num_impaired_domains == 0])))
    message(paste0("Impaired in 1 domain + moderate functional impairment: ", nrow(data[num_impaired_domains == 1 & moderate_function == 1])))
    message(paste0("Impaired in 1 domain + no moderate functional impairment: ", nrow(data[num_impaired_domains == 1 & moderate_function == 0])))
    message(paste0("Impaired in 1 domain + no moderate functional impairment + self-rated memory: ", nrow(data[num_impaired_domains == 1 & moderate_function == 0 & poormem == 1])))
    message(paste0("Impaired in 1 domain + no moderate functional impairment + no self-rated memory: ", nrow(data[num_impaired_domains == 1 & moderate_function == 0 & poormem == 0])))

    ## numbers for percentage figure 
    message(paste0("Percentage impaired in less than 2 domains ", sprintf("%.1f%%", (data[num_impaired_domains < 2, sum(weight)]) / data[, sum(weight)] * 100)))
    message(paste0("Percentage impaired in more than 2 domains ", sprintf("%.1f%%", (data[num_impaired_domains >= 2, sum(weight)]) / data[, sum(weight)] * 100)))
    message(paste0("Percentage severe functional impairment ", sprintf("%.1f%%", (data[num_impaired_domains >= 2 & severe_function == 1, sum(weight)]) / (data[num_impaired_domains >= 2, sum(weight)]) * 100)))
    message(paste0("Percentage without severe functional impairment ", sprintf("%.1f%%", (data[num_impaired_domains >= 2 & severe_function == 0, sum(weight)]) / (data[num_impaired_domains >= 2, sum(weight)]) * 100)))
    message(paste0("Percentage impaired in 1 domain ", sprintf("%.1f%%", (data[num_impaired_domains == 1, sum(weight)]) / (data[num_impaired_domains < 2, sum(weight)]) * 100)))
    message(paste0("Percentage not impaired in 1 domain ", sprintf("%.1f%%", (data[num_impaired_domains == 0, sum(weight)]) / (data[num_impaired_domains < 2, sum(weight)]) * 100)))
    message(paste0("Percentage moderate functional impairment ", sprintf("%.1f%%", (data[num_impaired_domains == 1 & moderate_function == 1, sum(weight)]) / (data[num_impaired_domains == 1, sum(weight)]) * 100)))
    message(paste0("Percentage without moderate functional impairment ", sprintf("%.1f%%", (data[num_impaired_domains == 1 & moderate_function == 0, sum(weight)]) / (data[num_impaired_domains == 1, sum(weight)]) * 100)))
    message(paste0("Percentage self-rated poor memory ", sprintf("%.1f%%", (data[num_impaired_domains == 1 & moderate_function == 0 & poormem == 1, sum(weight)]) / (data[num_impaired_domains == 1 & moderate_function == 0, sum(weight)]) * 100)))
    message(paste0("Percentage no self-rated poor memory ", sprintf("%.1f%%", (data[num_impaired_domains == 1 & moderate_function == 0 & poormem == 0, sum(weight)]) / (data[num_impaired_domains == 1 & moderate_function == 0, sum(weight)]) * 100)))

    ## calculate prevalence of dementia and MCI
    mean_design <- survey::svydesign(ids = ~1, weights = ~weight, data = data)
    dementia_prevalence <- survey::svyciprop(~dementia, design = mean_design)
    mci_prevalence <- survey::svyciprop(~mci, design = mean_design)
    message(paste0("Dementia prevalence: ", sprintf("%.1f%%", dementia_prevalence*100), " (", sprintf("%.1f%%", attr(dementia_prevalence, "ci")[[1]]*100), ", ", sprintf("%.1f%%", attr(dementia_prevalence, "ci")[[2]]*100), ")"))
    message(paste0("MCI prevalence: ", sprintf("%.1f%%", mci_prevalence*100), " (", sprintf("%.1f%%", attr(mci_prevalence, "ci")[[1]]*100), ", ", sprintf("%.1f%%", attr(mci_prevalence, "ci")[[2]]*100), ")"))

}

print_results(class_dt1)
print_results(class_dt2)

# MAKE COMPARISONS -----------------------------------------------------------

## prevalence of clinical dementia and MCI
clinical_dementia_prevalence <- survey::svyciprop(~I(diagnosis_3cat == "Dementia"), design = mean_design)
clinical_mci_prevalence <- survey::svyciprop(~I(diagnosis_3cat == "MCI"), design = mean_design)

## prevalence of clinical dementia and MCI (original ADAMS definition)
ADAMSorig_dementia_prevalence <- survey::svyciprop(~I(diagnosis_3cat_ADAMS == "Dementia"), design = mean_design)
ADAMSorig_cind_prevalence <- survey::svyciprop(~I(diagnosis_3cat_ADAMS == "CIND"), design = mean_design)

# SAVE RESULTS -----------------------------------------------------------------

final_data <- copy(class_dt2)
final_data <- merge(final_data, class_dt1[, .(ADAMSSID, predicted_3cat_s1 = predicted_3cat, predicted_2cat_s1 = predicted_2cat)], by = "ADAMSSID")

readr::write_rds(final_data, here::here(paste0(rds_filepath, "hcapalgo.rds")))

