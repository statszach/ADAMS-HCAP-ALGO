rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"; code_filepath <- "C:\\Users\\emmanich\\code\\ADAMS-HCAP-ALGO\\"
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

load(here::here(rds_filepath, "010_tidy-data.Rdata"))
fscores <- read_rds(paste0(rds_filepath, "fscores.rds"))

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

## normative sample is just those without dementia 
normative <- copy(data[diagnosis_3cat == "Normal"])

## blom transformed versions in normative sample
blom_transform <- function(x){
    ranks <- rank(x, na.last = "keep")
    return(qnorm((ranks-0.375)/(length(x)+0.25)))
}
normative[, paste0(domain_scores, "_b") := lapply(.SD, blom_transform), .SDcols = domain_scores]

# MODELS TO GET BLOM SCORES IN FULL SAMPLE ----------------------------------

## is orientation handled differently in Rich's code? (https://github.com/rnj0nes/HCAP22/blob/main/Norms-with-weights-202109/-111-standardize-cognition-scores.do#L296)
blom_design <- survey::svydesign(ids = ~1, weights = ~weight, data = normative)
blom_models <- lapply(domain_scores, function(score){
    quantiles <- survey::svyquantile(as.formula(paste0("~", score)), design = blom_design, quantiles = c(0.05, 0.35, 0.65, 0.95))
    knot_locations <- unique(quantiles[[1]][,1])
    knot_locations <- knot_locations[!knot_locations %in% c(normative[, min(get(score))], normative[, max(get(score))])] ## knots can't be the score min or max
    formula <- paste0(score, "_b ~ splines::ns(", score, ", knots = c(", paste(knot_locations, collapse = ", "), "))")
    return(survey::svyglm(formula, design = blom_design))
})

## get predicted blom in the full sample 
setDT(data) ## tbh I'm not totally sure why this is needed, but it has to do with shallow copies, and it gets rid of a warning for below
data[, paste0(domain_scores, "_b_pred") := 
       lapply(1:length(domain_scores), function(x) predict(blom_models[[x]], newdata = data))]

## add predicted blom scores to normative dataset (Q: use true blom transformed or prediction, currently prediction)
setDT(normative) ## tbh I'm not totally sure why this is needed, but it has to do with shallow copies, and it gets rid of a warning for below
normative[, paste0(domain_scores, "_b_pred") := 
            lapply(1:length(domain_scores), function(x) predict(blom_models[[x]], newdata = normative))]

## get SD of blom in normative sample 
blom_sds <- as.numeric(normative[, lapply(.SD, sd, na.rm = TRUE), .SDcols = paste0(domain_scores, "_b_pred")])

# NORMATIVE SAMPLE MODELS ---------------------------------------------------

## add all two-way interactions to adjustment vars
interaction_terms <- combn(adjust_vars, 2, FUN = function(x) paste0(x[1], ":", x[2]))

## normative models
normative_design <- survey::svydesign(ids = ~1, weights = ~weight, data = normative)
norms_models <- lapply(domain_scores, function(score){
    formula <- paste0(score, "_b_pred ~ ", paste(c(adjust_vars, interaction_terms), collapse = " + "))
    return(survey::svyglm(formula, design = normative_design))
})

## look at model parameters
model_params <- rbindlist(lapply(1:length(norms_models), 
                                 function(x){
                                             y <- as.data.table(broom::tidy(norms_models[[x]]))
                                             y[, domain := domain_scores[x]]
                                             return(y)}), 
                                             use.names = TRUE)                                             
## sanity check, these seem okay 

## get model r2s 
norms_r2 <- sapply(norms_models, function(x) 1 - (x$deviance / x$null.deviance))

# GENERATE IMPAIRMENT CLASSIFICATIONS ----------------------------------------

## expected cognition based on norms
data[, paste0(domain_scores, "_b_norms") := 
              lapply(1:length(domain_scores), function(x) predict(norms_models[[x]], newdata = data))]

## calculate T-scores
data[, paste0(domain_scores, "_T_score") := 
       lapply(1:length(domain_scores), 
       function(x) (50+
                    10*(get(paste0(domain_scores[x], "_b_pred"))-get(paste0(domain_scores[x], "_b_norms")))/
                    (blom_sds[x]*sqrt(1 - norms_r2[x]))))]

## check should have mean 50, sd 10 in normative sample 
data[diagnosis_3cat == "Normal", 
     lapply(.SD, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))), 
     .SDcols = paste0(domain_scores, "_T_score")]

## define impairment as lower than 36 for each domain 
data[, paste0(domain_scores, "_impairment") := 
       lapply(1:length(domain_scores), function(x) ifelse(get(paste0(domain_scores[x], "_T_score")) < 36, 1, 0))]

## number of impaired domains 
data[, num_impaired_domains := rowSums(.SD), .SDcols = paste0(domain_scores, "_impairment")]

# OTHER ALGORITHM CUT-POINTS ---------------------------------------------------

## need imputation model for iqcode - just using this simple one for now - we should probably add predictors
data <- simputation::impute_pmm(data, iqcode_mean ~ age + blessed_sum)

## define severe and moderate function loss according to cut points from Manly-Jones
data[, severe_function := as.numeric(iqcode_mean >= 3.4 | blessed_sum >= 2)]
data[, moderate_function := as.numeric(iqcode_mean > 3 | blessed_sum > 0)]

# DEFINE DEMENTIA AND MCI ------------------------------------------------------

data[, dementia := as.numeric(num_impaired_domains >= 2 & severe_function == 1)]
data[, mci := as.numeric((num_impaired_domains >= 2 & severe_function == 0) |
                         (num_impaired_domains == 1 & moderate_function == 1) | 
                         (num_impaired_domains == 1 & moderate_function == 0 | poormem == 1))]

## calculate prevalence of dementia and MCI
mean_design <- survey::svydesign(ids = ~1, weights = ~weight, data = data)
dementia_prevalence <- survey::svyciprop(~dementia, design = mean_design)
mci_prevalence <- survey::svyciprop(~mci, design = mean_design)

## predicted three-cat 
data[, predicted_3cat := factor(fcase(dementia == 1, "Dementia", 
                                      mci == 1, "MCI", 
                                      (!dementia == 1 & !mci == 1), "Normal"), 
                                levels = c("Dementia", "MCI", "Normal"))]

## let's look at things!  
data[, table(predicted_3cat, diagnosis_3cat)]
data[diagnosis_3cat == "Normal" & predicted_3cat == "Dementia", table(diagnosis)]
data[, chisq.test(table(predicted_3cat, diagnosis_3cat))]
data[, vcd::Kappa(table(predicted_3cat, diagnosis_3cat))]
